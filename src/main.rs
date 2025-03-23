mod fuzzy;
mod pdb_loader;
mod ui;

use clap::Parser;
use iced_x86::{
    Decoder, DecoderOptions, Formatter, FormatterOutput, FormatterTextKind, Instruction,
    IntelFormatter,
};
use patternsleuth_image::image::{Image, ImageBuilder};
use pdb::{FallibleIterator, SymbolData};
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use std::{error::Error, fmt};
use ui::{App, restore_terminal, run_app, setup_terminal};

// Custom error type for our disassembler
#[derive(Debug)]
pub enum DisassemblerError {
    Io(std::io::Error),
    Pdb(pdb::Error),
    Format(String),
}

impl fmt::Display for DisassemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisassemblerError::Io(e) => write!(f, "I/O error: {}", e),
            DisassemblerError::Pdb(e) => write!(f, "PDB error: {}", e),
            DisassemblerError::Format(s) => write!(f, "Format error: {}", s),
        }
    }
}

impl Error for DisassemblerError {}

impl From<std::io::Error> for DisassemblerError {
    fn from(err: std::io::Error) -> Self {
        DisassemblerError::Io(err)
    }
}

impl From<pdb::Error> for DisassemblerError {
    fn from(err: pdb::Error) -> Self {
        DisassemblerError::Pdb(err)
    }
}

// Struct to store symbol information
#[derive(Clone)]
pub struct SymbolInfo {
    address: u64,
    name: String,
    demangled: Option<String>,
    kind: SymbolKind,
}

#[derive(Clone)]
enum SymbolKind {
    Function,
    Data,
}

impl SymbolInfo {
    fn display_name(&self) -> &str {
        self.demangled.as_deref().unwrap_or(&self.name)
    }
}

// Function to load debug symbols from PDB file
fn load_pdb_symbols(image_base: u64, pdb_path: &Path) -> anyhow::Result<Vec<SymbolInfo>> {
    let file = fs::File::open(pdb_path)?;
    let mut pdb = pdb_loader::load(file)?;

    let symbol_table = pdb.global_symbols()?;
    let address_map = pdb.address_map()?;

    let mut symbols = Vec::new();
    let mut symbol_iter = symbol_table.iter();
    while let Some(symbol) = symbol_iter.next()? {
        if let Ok(SymbolData::Public(data)) = symbol.parse() {
            if let Some(rva) = data.offset.to_rva(&address_map) {
                symbols.push(SymbolInfo {
                    address: image_base + rva.0 as u64,
                    name: data.name.to_string().to_string(),
                    demangled: None,
                    kind: if data.function {
                        SymbolKind::Function
                    } else {
                        SymbolKind::Data
                    },
                });
            }
        } else if let Ok(SymbolData::Procedure(data)) = symbol.parse() {
            if let Some(rva) = data.offset.to_rva(&address_map) {
                symbols.push(SymbolInfo {
                    address: image_base + rva.0 as u64,
                    name: data.name.to_string().to_string(),
                    demangled: None,
                    kind: SymbolKind::Function,
                });
            }
        }
    }

    symbols.par_iter_mut().for_each(|sym| {
        sym.demangled =
            msvc_demangler::demangle(&sym.name, msvc_demangler::DemangleFlags::COMPLETE).ok();
    });

    Ok(symbols)
}

// CLI arguments using clap's derive feature
#[derive(Parser)]
#[clap(
    author,
    version,
    about = "Disassembles binaries with debug symbol annotation"
)]
struct Cli {
    /// Input binary file to disassemble
    #[clap(value_parser)]
    input: PathBuf,

    /// Path to PDB file for debug symbols
    #[clap(short, long, value_parser)]
    pdb: Option<PathBuf>,

    /// Start address for disassembly (hexadecimal)
    #[clap(short, long, value_parser = parse_hex_address)]
    address: Option<u64>,

    /// Symbol to disassemble
    #[clap(short, long)]
    symbol: Option<String>,

    /// Number of bytes to disassemble
    #[clap(short, long)]
    length: Option<usize>,
}

fn parse_hex_address(s: &str) -> Result<u64, String> {
    let s = s.trim_start_matches("0x");
    u64::from_str_radix(s, 16).map_err(|e| format!("Invalid hexadecimal address: {}", e))
}

// Define structured types for disassembly output
#[derive(Clone)]
pub enum DisassemblyLine {
    Empty,
    Symbol(SymbolInfo),
    Text(String),
    Instruction {
        address: u64,
        bytes: Vec<u8>,
        instruction: String,
        comments: Vec<DisassemblyComment>,
    },
}

#[derive(Clone)]
pub enum DisassemblyComment {
    BranchTarget(SymbolInfo),
    MemoryReference(SymbolInfo),
    Data(Vec<u8>),
}

// Simple formatter that doesn't add colors
struct PlainFormatterOutput<'a>(&'a mut String);

impl FormatterOutput for PlainFormatterOutput<'_> {
    fn write(&mut self, text: &str, _kind: FormatterTextKind) {
        self.0.push_str(text);
    }
}

// Add a new struct to hold the binary data
struct BinaryData<'data> {
    pub file: Image<'data>,
    pub symbols: Vec<SymbolInfo>,
    pub symbol_map: HashMap<u64, Vec<SymbolInfo>>,
}

impl<'data> BinaryData<'data> {
    pub fn new(
        data: &'data [u8],
        file_path: &Path,
        pdb_path: Option<&Path>,
    ) -> anyhow::Result<Self> {
        let file = ImageBuilder::default().build(data)?;

        // Load symbols from PDB if available, otherwise from object file
        let mut symbols = if let Some(pdb) = pdb_path {
            load_pdb_symbols(file.base_address as u64, pdb)?
        } else {
            vec![]
        };

        // Sort symbols by address for binary search
        symbols.sort_by_key(|s| s.address);

        // Create a map for quick symbol lookup
        let mut symbol_map: HashMap<u64, Vec<SymbolInfo>> = HashMap::new();
        for symbol in &symbols {
            symbol_map
                .entry(symbol.address)
                .or_default()
                .push(symbol.clone());
        }

        Ok(Self {
            file,
            symbols,
            symbol_map,
        })
    }

    pub fn get_symbols_at_address(&self, address: u64) -> Option<&Vec<SymbolInfo>> {
        self.symbol_map.get(&address)
    }
}

// Update the disassemble_range function to use the BinaryData
pub fn disassemble_range(
    binary_data: &BinaryData,
    address: u64,
    visible_lines: usize,
) -> Result<Vec<DisassemblyLine>, DisassemblerError> {
    let Some(section) = binary_data
        .file
        .memory
        .get_section_containing(address as usize)
        .ok()
    else {
        return Ok(vec![]);
    };

    let text_data = section.data();
    let text_address = section.address() as u64;

    // Calculate offset within section
    let offset = (address - text_address) as usize;

    // Determine how many bytes to disassemble
    // We'll disassemble more than needed to ensure we have enough lines
    let end = (offset + visible_lines * 16).min(text_data.len());

    // Create decoder
    let mut decoder = Decoder::with_ip(
        64, // TODO don't assume bitness
        &text_data[offset..end],
        text_address + offset as u64,
        DecoderOptions::NONE,
    );

    let mut formatter = IntelFormatter::new();
    formatter.options_mut().set_first_operand_char_index(10);

    // Create a vector to store structured disassembly lines
    let mut result = Vec::new();
    let mut instruction = Instruction::default();

    // Disassemble instructions until we have enough lines or can't decode anymore
    let mut line_count = 0;
    while decoder.can_decode() && line_count < visible_lines * 2 {
        // Decode instruction
        decoder.decode_out(&mut instruction);

        // Check if instruction address matches a known symbol
        let instr_address = instruction.ip();

        if let Some(syms) = binary_data.get_symbols_at_address(instr_address) {
            result.push(DisassemblyLine::Empty);
            line_count += 1;

            let mut iter = syms.iter();
            for sym in iter.by_ref().take(10) {
                result.push(DisassemblyLine::Symbol(sym.clone()));
                line_count += 1;
            }
            let count = iter.count();
            if count > 0 {
                result.push(DisassemblyLine::Text(format!(" ; ...{count} more")));
            }

            result.push(DisassemblyLine::Empty);
            line_count += 1;
        }

        // Format the instruction address and bytes
        let address = instruction.ip();
        let start_index = (instruction.ip() - text_address) as usize;
        let instr_bytes = &text_data[start_index..start_index + instruction.len()];

        // Format the instruction
        let mut instr_text = String::new();
        formatter.format(&instruction, &mut PlainFormatterOutput(&mut instr_text));

        // Create a structured instruction line
        let mut line = DisassemblyLine::Instruction {
            address,
            bytes: instr_bytes.to_vec(),
            instruction: instr_text,
            comments: Vec::new(),
        };

        // Add branch target comments if applicable
        let target = instruction.near_branch_target();
        if target != 0 {
            if let Some(syms) = binary_data.get_symbols_at_address(target) {
                for sym in syms {
                    if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                        comments.push(DisassemblyComment::BranchTarget(sym.clone()));
                    }
                }
            }
        }

        // Add memory reference comments if applicable
        if instruction.is_ip_rel_memory_operand() {
            let address = instruction.ip_rel_memory_address();

            if let Ok(data) = binary_data.file.memory.range_from(address as usize..) {
                let data = &data[..data.len().min(100)];
                if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                    comments.push(DisassemblyComment::Data(data.to_vec()));
                }
            }

            if let Some(syms) = binary_data.get_symbols_at_address(address) {
                for sym in syms {
                    if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                        comments.push(DisassemblyComment::MemoryReference(sym.clone()));
                    }
                }
            }
        }

        result.push(line);
        line_count += 1;
    }

    Ok(result)
}

// Update the main function to load the binary data once
fn main() -> Result<(), Box<dyn Error>> {
    color_eyre::install()?;

    let args = Cli::parse();

    let adj_pdb = args.input.with_extension("pdb");
    let pdb = if let Some(pdb) = args.pdb.as_deref() {
        Some(pdb)
    } else if adj_pdb.exists() {
        Some(adj_pdb.as_ref())
    } else {
        None
    };

    let data = std::fs::read(&args.input)?;

    // Load the binary data once
    let binary_data = BinaryData::new(&data, &args.input, pdb)?;

    // Get initial disassembly
    let (disassembly, start_address) =
        match get_initial_disassembly(&binary_data, args.address, args.symbol.as_deref()) {
            Ok(result) => result,
            Err(e) => {
                eprintln!("Error: {}", e);
                return Err(Box::new(e));
            }
        };

    // Set up the terminal UI
    let mut terminal = setup_terminal()?;

    // Create and initialize the app
    let mut app = App::new();
    app.set_disassembly(disassembly);
    app.set_symbols(binary_data.symbols.clone());
    app.set_binary_data(binary_data);
    app.set_current_address(start_address);

    // Run the app
    let tick_rate = Duration::from_millis(250);
    let app = run_app(&mut terminal, app, tick_rate)?;

    // Restore terminal
    restore_terminal(&mut terminal)?;

    // If the app should quit, exit normally
    if app.should_quit {
        Ok(())
    } else {
        // This shouldn't happen, but just in case
        Err("Application terminated unexpectedly".into())
    }
}

// Helper function to get the initial disassembly and starting address
fn get_initial_disassembly(
    binary_data: &BinaryData,
    address: Option<u64>,
    symbol: Option<&str>,
) -> Result<(Vec<DisassemblyLine>, u64), DisassemblerError> {
    let start_address = if let Some(addr) = address {
        addr
    } else if let Some(symbol_name) = symbol {
        // Find symbol by name
        let matches: Vec<_> = binary_data
            .symbols
            .iter()
            .filter(|s| {
                s.name.contains(symbol_name)
                    || s.demangled
                        .as_ref()
                        .is_some_and(|d| d.contains(symbol_name))
            })
            .collect();

        match matches.len() {
            0 => {
                return Err(DisassemblerError::Format(format!(
                    "No symbols matching {symbol_name:?} found"
                )));
            }
            1 => matches[0].address,
            _ => {
                // Multiple matches - return a list of matching symbols as disassembly lines
                let mut lines = Vec::new();
                lines.push(DisassemblyLine::Empty);
                lines.push(DisassemblyLine::Text(format!(
                    "Found multiple matches for {symbol_name:?}:"
                )));

                for sym in &matches {
                    lines.push(DisassemblyLine::Text(format!(
                        "{:X}: {}",
                        sym.address,
                        sym.demangled.as_ref().unwrap_or(&sym.name)
                    )));
                }

                // Return the first match's address
                return Ok((lines, matches[0].address));
            }
        }
    } else {
        // Default to the entry point or first section
        binary_data.file.base_address as u64
    };

    // Disassemble from the starting address
    let disassembly = disassemble_range(binary_data, start_address, 30).unwrap_or_default();

    Ok((disassembly, start_address))
}
