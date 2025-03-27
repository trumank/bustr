mod disassembly_ui;
mod fuzzy;
mod jump_render;
mod lazy_list;
mod pdb_loader;
mod search_ui;
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
    Empty {
        attached_to: u64,
    },
    Symbol {
        attached_to: u64,
        symbol: SymbolInfo,
    },
    Text {
        attached_to: u64,
        text: String,
    },
    Instruction {
        address: u64,
        bytes: Vec<u8>,
        instruction: String,
        comments: Vec<DisassemblyComment>,
        referenced_address: Option<u64>,
    },
}
impl DisassemblyLine {
    /// return address of line if instruction
    fn address(&self) -> Option<u64> {
        match self {
            DisassemblyLine::Instruction { address, .. } => Some(*address),
            _ => None,
        }
    }
    /// return address of line if instruction
    fn address_end(&self) -> Option<u64> {
        match self {
            DisassemblyLine::Instruction { address, bytes, .. } => {
                Some(*address + bytes.len() as u64)
            }
            _ => None,
        }
    }
    /// address of a block of lines
    /// multiple consecutive lines can belong to a single instruction line/address
    fn address_block(&self) -> u64 {
        match self {
            DisassemblyLine::Empty { attached_to } => *attached_to,
            DisassemblyLine::Symbol { attached_to, .. } => *attached_to,
            DisassemblyLine::Text { attached_to, .. } => *attached_to,
            DisassemblyLine::Instruction { address, .. } => *address,
        }
    }
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
pub struct BinaryData<'data> {
    pub file: Image<'data>,
    pub symbols: Vec<SymbolInfo>,
    pub symbol_map: HashMap<u64, Vec<SymbolInfo>>,
}

impl<'data> BinaryData<'data> {
    pub fn new(data: &'data [u8], pdb_path: Option<&Path>) -> anyhow::Result<Self> {
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
    decode_while: &mut dyn FnMut(&[DisassemblyLine]) -> bool,
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

    // Create decoder
    let mut decoder = Decoder::with_ip(
        64, // TODO don't assume bitness
        &text_data[offset..],
        text_address + offset as u64,
        DecoderOptions::NONE,
    );

    let mut formatter = IntelFormatter::new();
    formatter.options_mut().set_first_operand_char_index(10);

    // Create a vector to store structured disassembly lines
    let mut result = Vec::new();
    let mut instruction = Instruction::default();

    // Disassemble instructions until we have enough lines or can't decode anymore
    while decoder.can_decode() {
        // Decode instruction
        decoder.decode_out(&mut instruction);

        // Check if instruction address matches a known symbol
        let instr_address = instruction.ip();

        if let Some(syms) = binary_data.get_symbols_at_address(instr_address) {
            result.push(DisassemblyLine::Empty {
                attached_to: instr_address,
            });

            let mut iter = syms.iter();
            for sym in iter.by_ref().take(10) {
                result.push(DisassemblyLine::Symbol {
                    attached_to: instr_address,
                    symbol: sym.clone(),
                });
            }
            let count = iter.count();
            if count > 0 {
                result.push(DisassemblyLine::Text {
                    attached_to: instr_address,
                    text: format!(" ; ...{count} more"),
                });
            }

            result.push(DisassemblyLine::Empty {
                attached_to: instr_address,
            });
        }

        // Format the instruction address and bytes
        let address = instruction.ip();
        let start_index = (instruction.ip() - text_address) as usize;
        let instr_bytes = &text_data[start_index..start_index + instruction.len()];

        // Format the instruction
        let mut instr_text = String::new();
        formatter.format(&instruction, &mut PlainFormatterOutput(&mut instr_text));

        let branch_target = instruction.near_branch_target();
        let referenced_address = if instruction.is_ip_rel_memory_operand() {
            Some(instruction.ip_rel_memory_address())
        } else if branch_target != 0 {
            Some(branch_target)
        } else {
            None
        };

        // Create a structured instruction line
        let mut line = DisassemblyLine::Instruction {
            address,
            bytes: instr_bytes.to_vec(),
            instruction: instr_text,
            comments: Vec::new(),
            referenced_address,
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

        if !decode_while(&result) {
            break;
        }
    }

    Ok(result)
}

// Update the main function to load the binary data once
fn main() -> Result<(), Box<dyn Error>> {
    color_eyre::install()?;

    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        crossterm::execute!(std::io::stderr(), crossterm::terminal::LeaveAlternateScreen).unwrap();
        crossterm::terminal::disable_raw_mode().unwrap();
        hook(panic_info);
    }));

    use tracing_subscriber::prelude::*;
    tracing_subscriber::registry()
        .with(tui_logger::TuiTracingSubscriberLayer)
        .init();
    tui_logger::init_logger(tui_logger::LevelFilter::Trace).unwrap();

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
    let binary_data = BinaryData::new(&data, pdb)?;

    // Set up the terminal UI
    let mut terminal = setup_terminal()?;

    // Create and initialize the app
    let mut app = App::new();
    app.set_symbols(binary_data.symbols.clone());
    app.set_binary_data(binary_data);
    app.file_path = Some(args.input);

    // Run the app
    let tick_rate = Duration::from_millis(250);
    let app = run_app(&mut terminal, app, tick_rate)?;

    // Restore terminal
    restore_terminal(&mut terminal)?;

    let should_quit = app.should_quit;

    // dropping can take a long time so just don't
    std::mem::forget(app);

    // If the app should quit, exit normally
    if should_quit {
        Ok(())
    } else {
        // This shouldn't happen, but just in case
        Err("Application terminated unexpectedly".into())
    }
}
