mod fuzzy;
mod pdb_loader;
mod ui;

use clap::Parser;
use colored::{ColoredString, Colorize as _};
use iced_x86::{
    Decoder, DecoderOptions, Formatter, FormatterOutput, FormatterTextKind, Instruction,
    IntelFormatter,
};
use object::{File, Object, ObjectSection, ObjectSymbol};
use pdb::{FallibleIterator, PDB, SymbolData};
use rayon::prelude::*;
use std::borrow::Cow;
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
    Object(object::Error),
    Pdb(pdb::Error),
    Format(String),
}

impl fmt::Display for DisassemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisassemblerError::Io(e) => write!(f, "I/O error: {}", e),
            DisassemblerError::Object(e) => write!(f, "Object file error: {}", e),
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

impl From<object::Error> for DisassemblerError {
    fn from(err: object::Error) -> Self {
        DisassemblerError::Object(err)
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
    fn display_name(&self) -> ColoredString {
        let txt = if let Some(n) = &self.demangled {
            n
        } else {
            &self.name
        };
        match self.kind {
            SymbolKind::Function => colors::symbol_function(txt),
            SymbolKind::Data => colors::symbol_data(txt),
        }
    }
}

// Function to load debug symbols from PDB file
fn load_pdb_symbols(
    image_base: u64,
    pdb_path: &Path,
) -> Result<Vec<SymbolInfo>, DisassemblerError> {
    let file = fs::File::open(pdb_path)?;
    let mut pdb = PDB::open(pdb_loader::MemmapSource::new(file))?;

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

// Function to load symbols from object file
fn load_object_symbols(object_path: &Path) -> Result<Vec<SymbolInfo>, DisassemblerError> {
    let data = fs::read(object_path)?;
    let obj_file = File::parse(&*data)?;

    let mut symbols = Vec::new();
    for symbol in obj_file.symbols() {
        if symbol.kind() == object::SymbolKind::Text {
            symbols.push(SymbolInfo {
                address: symbol.address(),
                name: symbol.name()?.to_string(),
                demangled: None,
                kind: SymbolKind::Function,
            });
        }
    }

    Ok(symbols)
}

// Function to disassemble a binary file and return the disassembly as a vector of strings
fn disassemble_to_vec(
    file_path: &Path,
    pdb_path: Option<&Path>,
    start_address: Option<u64>,
    symbol: Option<String>,
    length: Option<usize>,
) -> Result<(Vec<DisassemblyLine>, Vec<SymbolInfo>), DisassemblerError> {
    // Read the executable file
    let data = fs::read(file_path)?;
    let obj_file = File::parse(&*data)?;

    // Load symbols from PDB if available, otherwise from object file
    let mut symbols = if let Some(pdb) = pdb_path {
        load_pdb_symbols(obj_file.relative_address_base(), pdb)?
    } else {
        load_object_symbols(file_path)?
    };

    let mut start_addr = start_address;

    if let Some(symbol_name) = symbol {
        let matches: Vec<_> = symbols
            .iter()
            .filter(|s| s.display_name().contains(&symbol_name))
            .collect();
        match &matches[..] {
            [] => {
                return Err(DisassemblerError::Format(format!(
                    "No symbols matching {symbol_name:?} found"
                )));
            }
            [symbol] => {
                start_addr = Some(symbol.address);
            }
            symbols => {
                todo!();
                //let mut result = Vec::new();
                //result.push(format!("Found multiple matches for {symbol_name:?}:"));
                //for sym in symbols {
                //    result.push(format!("{:X} {}", sym.address, sym.display_name()));
                //}
                //return Ok((result, Vec::new()));
            }
        }
    }

    // Sort symbols by address for binary search
    symbols.sort_by_key(|s| s.address);

    // Create a map for quick symbol lookup
    let mut symbol_map: HashMap<u64, Vec<&SymbolInfo>> = HashMap::new();
    for symbol in &symbols {
        symbol_map.entry(symbol.address).or_default().push(symbol);
    }

    let text_section = if let Some(start_address) = start_addr {
        let mut ret_section = None;
        for section in obj_file.sections() {
            let sec_address = section.address();
            let sec_data = section.data()?;
            let mem_range = sec_address..sec_address + sec_data.len() as u64;
            if mem_range.contains(&start_address) {
                ret_section = Some(section);
                break;
            }
        }
        ret_section.ok_or_else(|| {
            DisassemblerError::Format(format!("No sections contain address 0x{start_address:X}"))
        })
    } else {
        // Find the .text section
        obj_file
            .section_by_name(".text")
            .ok_or_else(|| DisassemblerError::Format("Text section not found".into()))
    }?;

    let text_data = text_section.data()?;
    let text_address = text_section.address();

    // Determine architecture and bitness
    let architecture = match obj_file.architecture() {
        object::Architecture::X86_64 => 64,
        object::Architecture::I386 => 32,
        arch => {
            return Err(DisassemblerError::Format(format!(
                "Unsupported architecture: {:?}",
                arch
            )));
        }
    };

    // Calculate the range to disassemble
    let start = start_addr.unwrap_or(0);
    let offset = if start >= text_address {
        (start - text_address) as usize
    } else {
        0
    };

    let end = if let Some(len) = length {
        (offset + len).min(text_data.len())
    } else {
        text_data.len()
    };

    if offset >= text_data.len() {
        return Err(DisassemblerError::Format(
            "Start address is outside of text section".into(),
        ));
    }

    // Create decoder
    let mut decoder = Decoder::with_ip(
        architecture,
        &text_data[offset..end],
        text_address + offset as u64,
        DecoderOptions::NONE,
    );

    let mut formatter = IntelFormatter::new();
    formatter.options_mut().set_first_operand_char_index(10);

    // Create a vector to store structured disassembly lines
    let mut result = Vec::new();
    let mut instruction = Instruction::default();

    // Disassemble instructions
    while decoder.can_decode() {
        // Decode instruction
        decoder.decode_out(&mut instruction);

        // Check if instruction address matches a known symbol
        let instr_address = instruction.ip();

        if let Some(syms) = symbol_map.get(&instr_address) {
            result.push(DisassemblyLine::Empty);
            for sym in syms {
                result.push(DisassemblyLine::Symbol((*sym).clone()));
            }
            result.push(DisassemblyLine::Empty);
        }

        // Format the instruction address and bytes
        let address = instruction.ip();
        let start_index = (instruction.ip() - text_address as u64) as usize;
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
            if let Some(syms) = symbol_map.get(&target) {
                for sym in syms {
                    if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                        comments.push(DisassemblyComment::BranchTarget((*sym).clone()));
                    }
                }
            }
        }

        // Add memory reference comments if applicable
        if instruction.is_ip_rel_memory_operand() {
            let address = instruction.ip_rel_memory_address();

            for section in obj_file.sections() {
                let sec_address = section.address();
                let sec_data = section.data()?;
                let mem_range = sec_address..sec_address + sec_data.len() as u64;
                if mem_range.contains(&address) {
                    let data = &sec_data[(address - sec_address) as usize
                        ..((address - sec_address) as usize + 100).min(sec_data.len())];

                    if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                        comments.push(DisassemblyComment::Data(data.to_vec()));
                    }
                    break;
                }
            }

            if let Some(syms) = symbol_map.get(&address) {
                for sym in syms {
                    if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                        comments.push(DisassemblyComment::MemoryReference((*sym).clone()));
                    }
                }
            }
        }

        result.push(line);
    }

    Ok((result, symbols.clone()))
}

/// Attempt to deduce type of a chunk of memory and print useful information about it
fn format_data(data: &[u8]) -> String {
    enum StrType {
        Str,
        WStr,
    }

    let str = {
        let wide_len = data
            .chunks(2)
            .position(|c| c == [0, 0] || !(c[1] == 0 && char::from(c[0]).is_ascii()))
            .unwrap_or(data.len());

        if wide_len > 5 {
            let chars = data
                .chunks(2)
                .map(|c| u16::from_le_bytes(c.try_into().unwrap()))
                .take(wide_len)
                .collect::<Vec<_>>();

            Some((StrType::WStr, Cow::Owned(String::from_utf16_lossy(&chars))))
        } else {
            let len = data
                .iter()
                .position(|c| *c == 0 || !char::from(*c).is_ascii())
                .unwrap_or(data.len());

            if len > 5 {
                Some((StrType::Str, String::from_utf8_lossy(&data[..len])))
            } else {
                None
            }
        }
    };

    let data = if let Some((t, str)) = str {
        colors::data_string(format!(
            "{}{str:?}",
            match t {
                StrType::Str => "",
                StrType::WStr => "L",
            }
        ))
    } else {
        let mut output = String::from("[");
        for b in data {
            output.push_str(&format!(" {b:02X}"));
        }
        output.push(']');
        colors::data_bytes(output)
    };
    format!(" -> {data}")
}

// Custom formatter output
struct MyFormatterOutput<'a>(&'a mut String);

impl FormatterOutput for MyFormatterOutput<'_> {
    fn write(&mut self, text: &str, kind: FormatterTextKind) {
        //self.0.push_str(&get_color(text, kind).to_string());
        self.0.push_str(text);
    }
}

fn get_color(s: &str, kind: FormatterTextKind) -> ColoredString {
    match kind {
        FormatterTextKind::Directive | FormatterTextKind::Keyword => s.bright_yellow(),
        FormatterTextKind::Prefix | FormatterTextKind::Mnemonic => s.bright_red(),
        FormatterTextKind::Register => s.bright_blue(),
        FormatterTextKind::Number => s.bright_cyan(),
        _ => s.white(),
    }
}

mod colors {
    use colored::{ColoredString, Colorize as _};

    pub fn symbol_function<S: AsRef<str>>(txt: S) -> ColoredString {
        txt.as_ref().bright_yellow()
    }
    pub fn symbol_data<S: AsRef<str>>(txt: S) -> ColoredString {
        txt.as_ref().bright_cyan()
    }
    pub fn data_string<S: AsRef<str>>(txt: S) -> ColoredString {
        txt.as_ref().bright_red()
    }
    pub fn data_bytes<S: AsRef<str>>(txt: S) -> ColoredString {
        txt.as_ref().bright_cyan()
    }
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

self_cell::self_cell!(
    struct OwnedFile {
        owner: Vec<u8>,

        #[covariant]
        dependent: File,
    }
);

// Add a new struct to hold the binary data
struct BinaryData {
    pub file: OwnedFile,
    pub symbols: Vec<SymbolInfo>,
    pub symbol_map: HashMap<u64, Vec<SymbolInfo>>,
}

impl BinaryData {
    pub fn new(file_path: &Path, pdb_path: Option<&Path>) -> Result<Self, DisassemblerError> {
        let data = fs::read(file_path)?;
        let file = OwnedFile::try_new(data, |data| File::parse(&*data))?;
        let obj_file = file.borrow_dependent();

        // Load symbols from PDB if available, otherwise from object file
        let mut symbols = if let Some(pdb) = pdb_path {
            load_pdb_symbols(obj_file.relative_address_base(), pdb)?
        } else {
            load_object_symbols(file_path)?
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
    let obj_file = &binary_data.file.borrow_dependent();

    // Find the section containing the address
    let mut text_section = None;

    for section in obj_file.sections() {
        let sec_address = section.address();
        let sec_size = section.data()?.len();
        let mem_range = sec_address..(sec_address + sec_size as u64);

        if mem_range.contains(&address) {
            text_section = Some(section);
            break;
        }
    }

    let Some(section) = text_section else {
        return Ok(vec![]);
    };

    let text_data = section.data()?;
    let text_address = section.address();

    // Calculate offset within section
    let offset = (address - text_address) as usize;

    // Determine how many bytes to disassemble
    // We'll disassemble more than needed to ensure we have enough lines
    let end = (offset + visible_lines * 16).min(text_data.len());

    // Determine architecture
    let architecture = match obj_file.architecture() {
        object::Architecture::X86_64 => 64,
        object::Architecture::I386 => 32,
        arch => {
            return Err(DisassemblerError::Format(format!(
                "Unsupported architecture: {:?}",
                arch
            )));
        }
    };

    // Create decoder
    let mut decoder = Decoder::with_ip(
        architecture,
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
        let start_index = (instruction.ip() - text_address as u64) as usize;
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

            for section in obj_file.sections() {
                let sec_address = section.address();
                let sec_data = section.data()?;
                let mem_range = sec_address..sec_address + sec_data.len() as u64;
                if mem_range.contains(&address) {
                    let data = &sec_data[(address - sec_address) as usize
                        ..((address - sec_address) as usize + 100).min(sec_data.len())];

                    if let DisassemblyLine::Instruction { comments, .. } = &mut line {
                        comments.push(DisassemblyComment::Data(data.to_vec()));
                    }
                    break;
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

    // Load the binary data once
    let binary_data = match BinaryData::new(&args.input, pdb) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error loading binary: {}", e);
            return Err(Box::new(e));
        }
    };

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
                        .map_or(false, |d| d.contains(symbol_name))
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
        binary_data.file.borrow_dependent().entry()
    };

    // Disassemble from the starting address
    let disassembly = disassemble_range(binary_data, start_address, 30).unwrap_or_default();

    Ok((disassembly, start_address))
}
