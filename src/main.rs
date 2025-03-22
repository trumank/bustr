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
enum DisassemblerError {
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
) -> Result<(Vec<String>, Vec<SymbolInfo>), DisassemblerError> {
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
                let mut result = Vec::new();
                result.push(format!("Found multiple matches for {symbol_name:?}:"));
                for sym in symbols {
                    result.push(format!("{:X} {}", sym.address, sym.display_name()));
                }
                return Ok((result, Vec::new()));
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

    // Buffer for formatted instructions
    let mut result = Vec::new();
    let mut instruction = Instruction::default();

    // Disassemble instructions
    while decoder.can_decode() {
        let mut output = String::new();

        // Decode instruction
        decoder.decode_out(&mut instruction);

        // Check if instruction address matches a known symbol
        let instr_address = instruction.ip();

        if let Some(syms) = symbol_map.get(&instr_address) {
            result.push(String::new());
            for sym in syms {
                result.push(format!(" ; {}", sym.display_name()));
            }
            result.push(String::new());
        }

        // Format the instruction address
        output.push_str(&format!("{:016X} ", instruction.ip()));
        let start_index = (instruction.ip() - text_address as u64) as usize;
        let instr_bytes = &text_data[start_index..start_index + instruction.len()];
        for b in instr_bytes.iter() {
            output.push_str(&format!("{:02X} ", b));
        }

        // Pad for alignment
        for _ in instruction.len()..12 {
            output.push_str("   ");
        }

        // Format the instruction
        formatter.format(&instruction, &mut MyFormatterOutput(&mut output));

        // Add inline symbols if in that mode
        let target = instruction.near_branch_target();
        if target != 0 {
            if let Some(syms) = symbol_map.get(&target) {
                output.push_str(" ; -> ");
                for sym in syms {
                    output.push_str(&format!(" // {}", sym.display_name()));
                }
            }
        }

        // Find any interesting data this instruction is pointing to (e.g. strings)
        if instruction.is_ip_rel_memory_operand() {
            let address = instruction.ip_rel_memory_address();

            for section in obj_file.sections() {
                let sec_address = section.address();
                let sec_data = section.data()?;
                let mem_range = sec_address..sec_address + sec_data.len() as u64;
                if mem_range.contains(&address) {
                    let data = &sec_data[(address - sec_address) as usize
                        ..((address - sec_address) as usize + 100).min(sec_data.len())];

                    output.push_str("; ");
                    output.push_str(&format_data(data));
                    break;
                }
            }

            if let Some(syms) = symbol_map.get(&address) {
                output.push_str(" ; -> ");
                for sym in syms {
                    output.push_str(&format!(" // {}", sym.display_name()));
                }
            }
        }

        result.push(output);
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

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();

    let adj_pdb = args.input.with_extension("pdb");
    let pdb = if let Some(pdb) = args.pdb.as_deref() {
        Some(pdb)
    } else if adj_pdb.exists() {
        Some(adj_pdb.as_ref())
    } else {
        None
    };

    // Get the disassembly as a vector of strings
    let (disassembly, symbols) = match disassemble_to_vec(
        &args.input,
        pdb,
        args.address,
        args.symbol.clone(),
        args.length,
    ) {
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
    app.set_symbols(symbols);

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
