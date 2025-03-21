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
use std::sync::Arc;
use std::{error::Error, fmt};

// Custom error type for our disassembler
#[derive(Debug)]
enum DisassemblerError {
    IoError(std::io::Error),
    ObjectError(object::Error),
    PdbError(pdb::Error),
    FormatError(String),
}

impl fmt::Display for DisassemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisassemblerError::IoError(e) => write!(f, "I/O error: {}", e),
            DisassemblerError::ObjectError(e) => write!(f, "Object file error: {}", e),
            DisassemblerError::PdbError(e) => write!(f, "PDB error: {}", e),
            DisassemblerError::FormatError(s) => write!(f, "Format error: {}", s),
        }
    }
}

impl Error for DisassemblerError {}

impl From<std::io::Error> for DisassemblerError {
    fn from(err: std::io::Error) -> Self {
        DisassemblerError::IoError(err)
    }
}

impl From<object::Error> for DisassemblerError {
    fn from(err: object::Error) -> Self {
        DisassemblerError::ObjectError(err)
    }
}

impl From<pdb::Error> for DisassemblerError {
    fn from(err: pdb::Error) -> Self {
        DisassemblerError::PdbError(err)
    }
}

// Struct to store symbol information
struct SymbolInfo {
    address: u64,
    name: String,
    demangled: Option<String>,
}
impl SymbolInfo {
    fn display_name(&self) -> &str {
        if let Some(n) = &self.demangled {
            n
        } else {
            &self.name
        }
    }
}

#[derive(Clone)]
struct MemmapSource<'data> {
    data: &'data [u8],
}
struct MemmapSourceView {
    data: Vec<u8>,
}
impl std::fmt::Debug for MemmapSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MemmapSource").finish_non_exhaustive()
    }
}
impl std::fmt::Debug for MemmapSourceView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("MemmapSourceView").finish_non_exhaustive()
    }
}
impl<'s> pdb::SourceView<'s> for MemmapSourceView {
    fn as_slice(&self) -> &[u8] {
        &self.data
    }
}

impl<'s> pdb::Source<'s> for MemmapSource<'s> {
    fn view(
        &mut self,
        slices: &[pdb::SourceSlice],
    ) -> Result<Box<dyn pdb::SourceView<'s>>, std::io::Error> {
        let len = slices.iter().fold(0, |acc, s| acc + s.size);

        let mut v = MemmapSourceView { data: vec![0; len] };

        {
            let bytes = v.data.as_mut_slice();
            let mut output_offset: usize = 0;

            //let continuous = slices.windows(2).all(|pair| {
            //    pair[0].offset + pair[0].size as u64 == pair[1].offset
            //});
            //dbg!((continuous, len));

            for slice in slices {
                if slice.offset % 4096 != 0 {
                    println!("{:X}", slice.offset);
                }
                bytes[output_offset..(output_offset + slice.size)].copy_from_slice(
                    &self.data[slice.offset as usize..slice.offset as usize + slice.size],
                );
                output_offset += slice.size;
            }
        }

        Ok(Box::new(v))
    }
}

// Function to load debug symbols from PDB file
fn load_pdb_symbols(
    image_base: u64,
    pdb_path: &Path,
) -> Result<Vec<SymbolInfo>, DisassemblerError> {
    let file = fs::File::open(pdb_path)?;
    use memmap2::Mmap;
    let mmap = unsafe { Mmap::map(&file)? };
    let mut pdb = PDB::open(MemmapSource { data: &mmap })?;

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
                });
            }
        } else if let Ok(SymbolData::Procedure(data)) = symbol.parse() {
            if let Some(rva) = data.offset.to_rva(&address_map) {
                symbols.push(SymbolInfo {
                    address: image_base + rva.0 as u64,
                    name: data.name.to_string().to_string(),
                    demangled: None,
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
            });
        }
    }

    Ok(symbols)
}

// Function to disassemble a binary file
fn disassemble(
    file_path: &Path,
    pdb_path: Option<&Path>,
    mut start_address: Option<u64>,
    symbol: Option<String>,
    length: Option<usize>,
) -> Result<(), DisassemblerError> {
    // Read the executable file
    let data = fs::read(file_path)?;
    let obj_file = File::parse(&*data)?;

    // Load symbols from PDB if available, otherwise from object file
    let mut symbols = if let Some(pdb) = pdb_path {
        load_pdb_symbols(obj_file.relative_address_base(), pdb)?
    } else {
        load_object_symbols(file_path)?
    };

    if let Some(symbol) = symbol {
        let matches: Vec<_> = symbols
            .iter()
            .filter(|s| s.display_name().contains(&symbol))
            .collect();
        match &matches[..] {
            [] => {
                println!("No symbols matching {symbol:?} found");
                return Ok(());
            }
            [symbol] => {
                start_address = Some(symbol.address);
            }
            symbols => {
                println!("Found multiple matches for {symbol:?}:");
                for sym in symbols {
                    println!("{:X} {}", sym.address, colors::symbol(sym.display_name()));
                }
                return Ok(());
            }
        }
    }

    // Sort symbols by address for binary search
    symbols.sort_by_key(|s| s.address);

    // Create a map for quick symbol lookup
    let mut symbol_map = HashMap::new();
    for symbol in &symbols {
        symbol_map.insert(symbol.address, symbol);
    }

    // Find the .text section
    let text_section = obj_file
        .section_by_name(".text")
        .ok_or_else(|| DisassemblerError::FormatError("Text section not found".into()))?;

    let text_data = text_section.data()?;
    let text_address = text_section.address();

    // Determine architecture and bitness
    let architecture = match obj_file.architecture() {
        object::Architecture::X86_64 => 64,
        object::Architecture::I386 => 32,
        arch => {
            return Err(DisassemblerError::FormatError(
                format!("Unsupported architecture: {:?}", arch).into(),
            ));
        }
    };

    // Calculate the range to disassemble
    let start = start_address.unwrap_or(0);
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
        return Err(DisassemblerError::FormatError(
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

    //formatter.options_mut().set_digit_separator("`");
    formatter.options_mut().set_first_operand_char_index(10);

    // Buffer for formatted instructions
    let mut output = String::new();
    let mut instruction = Instruction::default();

    // Disassemble instructions
    while decoder.can_decode() {
        output.clear();

        // Decode instruction
        decoder.decode_out(&mut instruction);

        // Check if instruction address matches a known symbol
        let instr_address = instruction.ip();

        if let Some(sym) = symbol_map.get(&instr_address) {
            println!();
            println!(" ; {}", colors::symbol(sym.display_name()));
            println!();
        }

        //// Find the symbol that is closest to but not greater than the instruction address
        //let symbol_name = match symbol_format {
        //    "inline" => {
        //        if let Some(name) = symbol_map.get(&instr_address) {
        //            symbol_found = true;
        //            format!(" ; <{}>", name)
        //        } else {
        //            String::new()
        //        }
        //    }
        //    "full" => {
        //        // Binary search to find the closest symbol before this address
        //        match symbols.binary_search_by_key(&instr_address, |s| s.address) {
        //            Ok(idx) => {
        //                // Exact match
        //                symbol_found = true;
        //                format!("\n{}:\n", symbols[idx].name)
        //            }
        //            Err(0) => String::new(), // No symbol before this address
        //            Err(idx) => {
        //                // Get the previous symbol
        //                let prev_symbol = &symbols[idx - 1];
        //                if instr_address == prev_symbol.address {
        //                    symbol_found = true;
        //                    format!("\n{}:\n", prev_symbol.name)
        //                } else {
        //                    String::new()
        //                }
        //            }
        //        }
        //    }
        //    _ => String::new(),
        //};

        //// Only print symbol headers for the first instruction at that address
        //if symbol_found {
        //    output.push_str(&symbol_name);
        //}

        // Format the instruction address
        print!("{:016X} ", instruction.ip());
        let start_index = (instruction.ip() - text_address as u64) as usize;
        let instr_bytes = &text_data[start_index..start_index + instruction.len()];
        for b in instr_bytes.iter() {
            print!("{:02X} ", b);
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
            if let Some(sym) = symbol_map.get(&target) {
                output.push_str(&format!(" ; -> {}", colors::symbol(sym.display_name())));
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
        }

        println!("{}", output);
        //output.push('\n');
    }

    // Print the disassembly
    //println!("{}", output);

    Ok(())
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

impl<'a> FormatterOutput for MyFormatterOutput<'a> {
    fn write(&mut self, text: &str, kind: FormatterTextKind) {
        self.0.push_str(&get_color(text, kind).to_string());
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

    pub fn symbol<S: AsRef<str>>(txt: S) -> ColoredString {
        txt.as_ref().bright_yellow()
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
    } else if std::fs::exists(&adj_pdb)? {
        Some(adj_pdb.as_ref())
    } else {
        None
    };

    disassemble(&args.input, pdb, args.address, args.symbol, args.length)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
}
