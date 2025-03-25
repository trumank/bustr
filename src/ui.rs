use crate::{
    BinaryData, DisassemblyComment, DisassemblyLine, SymbolInfo, SymbolKind,
    fuzzy::highlight_matches,
    lazy_list::{LazyList, ListItemProducer},
};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind, KeyModifiers,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use nucleo::pattern::{CaseMatching, Normalization};
use patternsleuth_image::PatternConfig;
use ratatui::{
    Frame, Terminal,
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph},
};
use std::{
    collections::VecDeque,
    error::Error,
    io,
    path::PathBuf,
    sync::mpsc,
    time::{Duration, Instant},
};

#[derive(Debug, Clone)]
struct NavigationEntry {
    address: u64,
    scroll_offset: usize,
}

#[derive(Clone)]
struct LogEntry {
    timestamp: Instant,
    message: String,
}

#[derive(Clone)]
pub struct Log(mpsc::Sender<LogEntry>);

impl Log {
    pub fn log(&mut self, message: impl Into<String>) {
        self.0
            .send(LogEntry {
                timestamp: Instant::now(),
                message: message.into(),
            })
            .unwrap();
    }
}

pub struct App<'data> {
    pub disassembly: Vec<DisassemblyLine>,
    pub symbols: Vec<SymbolInfo>,
    pub show_help: bool,
    pub should_quit: bool,
    pub show_log: bool,

    pub active_pane: Pane,
    pub search_mode: bool,

    pub disassembly_state: ListState,
    pub symbol_list_state: ListState,
    pub current_address: u64,
    pub file_path: Option<PathBuf>,
    pub pdb_path: Option<PathBuf>,
    pub needs_refresh: bool,
    pub binary_data: Option<BinaryData<'data>>,
    pub search_query: String,
    nucleo: nucleo::Nucleo<SymbolInfo>,
    pub xref_mode: bool,
    pub xref_query: String,
    pub xrefs: Vec<XRefInfo>,
    pub xref_list_state: ListState,
    pub goto_mode: bool,
    pub goto_query: String,

    navigation_stack: Vec<NavigationEntry>,
    navigation_index: usize,

    log_entries: VecDeque<LogEntry>,
    log_state: ListState,
    log: Log,
    log_rx: mpsc::Receiver<LogEntry>,
}

#[derive(PartialEq)]
pub enum Pane {
    Disassembly,
    Symbols,
    XRefs,
    DebugLog,
}

#[derive(Clone)]
pub struct XRefInfo {
    pub from_address: u64,
    pub to_address: u64,
    pub kind: XRefKind,
    pub instruction: String,
}

#[derive(Clone, PartialEq)]
pub enum XRefKind {
    Call,
    Jump,
    DataRef,
}

impl<'data> App<'data> {
    pub fn new() -> Self {
        let mut disassembly_state = ListState::default();
        disassembly_state.select(Some(0));

        let (log_tx, log_rx) = mpsc::channel();
        let log = Log(log_tx);

        Self {
            disassembly: Vec::new(),
            symbols: Vec::new(),
            show_help: false,
            should_quit: false,
            show_log: true,

            active_pane: Pane::Disassembly,
            search_mode: false,

            disassembly_state,
            symbol_list_state: ListState::default(),
            current_address: 0,
            file_path: None,
            pdb_path: None,
            needs_refresh: false,
            binary_data: None,
            search_query: String::new(),
            nucleo: nucleo::Nucleo::new(
                nucleo::Config::DEFAULT,
                std::sync::Arc::new(|| { /* erm */ }),
                None,
                1,
            ),
            xref_mode: false,
            xref_query: String::new(),
            xrefs: Vec::new(),
            xref_list_state: ListState::default(),
            goto_mode: false,
            goto_query: String::new(),

            navigation_stack: Vec::new(),
            navigation_index: 0,

            log_entries: VecDeque::with_capacity(1000),
            log_state: ListState::default(),
            log,
            log_rx,
        }
    }

    pub fn set_disassembly(&mut self, disassembly: Vec<DisassemblyLine>) {
        self.disassembly = disassembly;
    }

    pub fn set_symbols(&mut self, symbols: Vec<SymbolInfo>) {
        self.symbols = symbols;
        self.symbol_list_state.select(Some(0));
        for sym in &self.symbols {
            self.nucleo.injector().push(sym.clone(), |sym, columns| {
                columns[0] = sym.display_name().into();
            });
        }
    }

    pub fn set_xrefs(&mut self, xrefs: Vec<XRefInfo>) {
        self.xrefs = xrefs;
        self.xref_list_state.select(Some(0));
    }

    pub fn set_current_address(&mut self, address: u64) {
        self.current_address = address;
        self.needs_refresh = true;

        // Only push to navigation stack if this is a new navigation (not from history)
        if !self.navigation_stack.is_empty() {
            self.navigation_index += 1;
        }
        let entry = NavigationEntry {
            address,
            scroll_offset: self.disassembly_state.offset(),
        };
        if let Some(existing) = self.navigation_stack.get_mut(self.navigation_index) {
            *existing = entry;
        } else {
            self.navigation_stack.push(entry);
        }
        self.log.log(format!(
            "nav # {} stack {:x?}",
            self.navigation_index, self.navigation_stack
        ));
    }

    pub fn set_binary_data(&mut self, binary_data: BinaryData<'data>) {
        self.binary_data = Some(binary_data);
    }

    pub fn scroll_up(&mut self, amount: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                if let Some(mut selected) = self.disassembly_state.selected() {
                    // ensure margin of 10 off screen
                    if selected > 10 + amount {
                        selected -= amount;
                    } else if !self.disassembly.is_empty() {
                        let new_lines = self.prepend_disassembly(amount + 3);

                        selected -= amount - new_lines;
                        *self.disassembly_state.offset_mut() += new_lines;
                    }
                    self.disassembly_state.select(Some(selected));

                    // Update the current navigation entry's scroll offset
                    if let Some(entry) = self.navigation_stack.get_mut(self.navigation_index) {
                        entry.scroll_offset = self.disassembly_state.offset();
                        entry.address = self.disassembly[selected].address_block();
                    }
                    self.log.log(format!(
                        "nav # {} stack {:x?}",
                        self.navigation_index, self.navigation_stack
                    ));
                }
            }
            Pane::Symbols => {
                self.symbol_list_state.scroll_up_by(amount as u16);
                self.select_symbol();
            }
            Pane::XRefs => {
                self.xref_list_state.scroll_up_by(amount as u16);
                self.select_xref();
            }
            Pane::DebugLog => {
                self.log_state.scroll_up_by(amount as u16);
            }
        }
    }

    pub fn scroll_down(&mut self, amount: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                if let Some(mut selected) = self.disassembly_state.selected() {
                    let max = self.disassembly.len().saturating_sub(1);
                    if selected + amount < max {
                        selected += amount;
                    } else {
                        if !self.disassembly.is_empty() {
                            self.append_disassembly(amount + 3);
                        }
                        let max = self.disassembly.len().saturating_sub(1);
                        selected = (selected + amount).min(max);
                    }
                    self.disassembly_state.select(Some(selected));

                    // Update the current navigation entry's scroll offset
                    if let Some(entry) = self.navigation_stack.get_mut(self.navigation_index) {
                        entry.scroll_offset = self.disassembly_state.offset();
                        entry.address = self.disassembly[selected].address_block();
                    }
                    self.log.log(format!(
                        "nav # {} stack {:x?}",
                        self.navigation_index, self.navigation_stack
                    ));
                }
            }
            Pane::Symbols => {
                self.symbol_list_state.scroll_down_by(amount as u16);
                self.select_symbol();
            }
            Pane::XRefs => {
                self.xref_list_state.scroll_down_by(amount as u16);
                self.select_xref();
            }
            Pane::DebugLog => {
                self.log_state.scroll_down_by(amount as u16);
            }
        }
    }

    pub fn toggle_help(&mut self) {
        self.show_help = !self.show_help;
    }

    pub fn toggle_pane(&mut self) {
        self.active_pane = match self.active_pane {
            Pane::Disassembly => Pane::Symbols,
            Pane::Symbols => Pane::XRefs,
            Pane::XRefs => {
                if self.show_log {
                    Pane::DebugLog
                } else {
                    Pane::Disassembly
                }
            }
            Pane::DebugLog => Pane::Disassembly,
        };
    }

    pub fn select_symbol(&mut self) {
        if self.active_pane == Pane::Symbols {
            if let Some(idx) = self.symbol_list_state.selected() {
                if self.search_mode {
                    let snapshot = self.nucleo.snapshot();

                    if let Some(item) = snapshot.get_matched_item(idx as u32) {
                        self.set_current_address(item.data.address);
                    }
                } else if idx < self.symbols.len() {
                    //self.selected_symbol_index = Some(idx);
                    let symbol = &self.symbols[idx];
                    self.set_current_address(symbol.address);
                }
            }
        }
    }

    fn find_previous_address(&self) -> Option<u64> {
        self.disassembly
            .iter()
            .rev()
            .find_map(|l| l.address().filter(|a| *a < self.current_address))
    }

    fn find_next_address(&self) -> Option<u64> {
        self.disassembly
            .iter()
            .find_map(|l| l.address().filter(|a| *a > self.current_address))
    }

    // Method to jump to the top of the current pane
    pub fn jump_to_top(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                // For disassembly, we need to find the earliest address
                if let Some(first_addr) = self.find_first_address() {
                    self.set_current_address(first_addr);
                }
                *self.disassembly_state.offset_mut() = 0;
                self.disassembly_state.select(Some(0));
            }
            Pane::Symbols => {
                jump_to_top(&mut self.symbol_list_state);
                self.select_symbol();
            }
            Pane::XRefs => {
                jump_to_top(&mut self.xref_list_state);
                self.select_xref();
            }
            Pane::DebugLog => {
                if !self.log_entries.is_empty() {
                    self.log_state.select(Some(0));
                }
            }
        }
    }

    // Method to jump to the bottom of the current pane
    pub fn jump_to_bottom(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                // For disassembly, we need to find the latest address
                if let Some(last_addr) = self.find_last_address() {
                    self.set_current_address(last_addr);
                }
                // After refresh, we'll be at the top of the new view, so we need to scroll down
                self.needs_refresh = true;
            }
            Pane::Symbols => {
                // TODO handle search
                jump_to_bottom(&mut self.xref_list_state, self.symbols.len());
                self.select_symbol();
            }
            Pane::XRefs => {
                jump_to_bottom(&mut self.xref_list_state, self.xrefs.len());
                self.select_xref();
            }
            Pane::DebugLog => {
                if !self.log_entries.is_empty() {
                    self.log_state.select(Some(self.log_entries.len() - 1));
                }
            }
        }
    }

    // Helper to find the first address in the binary
    fn find_first_address(&self) -> Option<u64> {
        self.binary_data
            .as_ref()
            .and_then(|b| b.file.memory.sections().next())
            .map(|s| s.address() as u64)
    }

    // Helper to find the last address in the binary
    fn find_last_address(&self) -> Option<u64> {
        self.binary_data
            .as_ref()
            .and_then(|b| b.file.memory.sections().last())
            .map(|s| (s.address() + s.data().len()) as u64)
    }

    // Add methods for search functionality
    pub fn toggle_search(&mut self) {
        if self.active_pane == Pane::Symbols {
            self.search_mode = !self.search_mode;
            if self.search_mode {
                self.search_query.clear();
                self.update_filtered_symbols(false);
            }
        }
    }

    pub fn add_to_search(&mut self, c: char) {
        if self.search_mode {
            self.search_query.push(c);
            self.update_filtered_symbols(true);
        }
    }

    pub fn backspace_search(&mut self) {
        if self.search_mode && !self.search_query.is_empty() {
            self.search_query.pop();
            self.update_filtered_symbols(false);
        }
    }

    pub fn clear_search(&mut self) {
        if self.search_mode {
            self.search_query.clear();
            self.update_filtered_symbols(false);
        }
    }

    pub fn update_filtered_symbols(&mut self, append: bool) {
        // Perform fuzzy search
        let query = self.search_query.to_lowercase();

        self.nucleo
            .pattern
            .reparse(0, &query, CaseMatching::Smart, Normalization::Smart, append);

        self.nucleo.tick(10);

        // Reset scroll position
        jump_to_top(&mut self.symbol_list_state);
    }

    // Add methods for XRef functionality
    pub fn toggle_xref_mode(&mut self) {
        if self.active_pane == Pane::XRefs {
            self.xref_mode = !self.xref_mode;
            if self.xref_mode {
                self.xref_query.clear();
            }
        }
    }

    pub fn add_to_xref_query(&mut self, c: char) {
        if self.xref_mode {
            self.xref_query.push(c);
        }
    }

    pub fn backspace_xref_query(&mut self) {
        if self.xref_mode && !self.xref_query.is_empty() {
            self.xref_query.pop();
        }
    }

    pub fn submit_xref_query(&mut self) {
        if self.xref_mode && !self.xref_query.is_empty() {
            // Parse the address from the query
            if let Ok(addr) = u64::from_str_radix(self.xref_query.trim_start_matches("0x"), 16) {
                self.find_xrefs(addr);
                self.xref_mode = false;
            }
        }
    }

    pub fn find_xrefs(&mut self, address: u64) {
        let Some(binary_data) = &self.binary_data else {
            return;
        };

        let config = [PatternConfig::xref(
            (),
            "".into(),
            None,
            patternsleuth_image::scanner::Xref(address as usize),
        )];

        let res = binary_data.file.scan(&config).unwrap();

        self.set_xrefs(
            res.results
                .into_iter()
                .map(|(_, r)| XRefInfo {
                    from_address: r.address as u64,
                    to_address: address,
                    kind: XRefKind::Call,
                    instruction: format!("call 0x{:X}", address),
                })
                .collect(),
        )
    }

    pub fn select_xref(&mut self) {
        if self.active_pane == Pane::XRefs {
            if let Some(idx) = self.xref_list_state.selected() {
                if idx < self.xrefs.len() {
                    let xref = &self.xrefs[idx];
                    self.set_current_address(xref.from_address);
                }
            }
        }
    }

    // Add methods for goto functionality
    pub fn toggle_goto_mode(&mut self) {
        self.goto_mode = !self.goto_mode;
        if self.goto_mode {
            self.goto_query.clear();
        }
    }

    pub fn add_to_goto_query(&mut self, c: char) {
        if self.goto_mode {
            self.goto_query.push(c);
        }
    }

    pub fn backspace_goto_query(&mut self) {
        if self.goto_mode && !self.goto_query.is_empty() {
            self.goto_query.pop();
        }
    }

    pub fn submit_goto_query(&mut self) {
        if self.goto_mode && !self.goto_query.is_empty() {
            // Parse the address from the query
            if let Ok(addr) = u64::from_str_radix(self.goto_query.trim_start_matches("0x"), 16) {
                self.set_current_address(addr);
                self.active_pane = Pane::Disassembly;
                self.goto_mode = false;
            }
        }
    }

    /// load at least new_lines and prepend them to the current disassembly
    /// return how many lines were actually added
    /// also replaces first N lines because they could be improperly disassembled
    pub fn prepend_disassembly(&mut self, new_lines: usize) -> usize {
        if let Some(binary_data) = &self.binary_data {
            const REPLACE_BYTES: usize = 100;

            let mut added_lines = 0;

            while added_lines <= new_lines {
                let mut block_iter = iter_blocks(&self.disassembly);

                // find first block so we know how far back to start disassembling
                if let Some(first_block) = block_iter.next() {
                    let first_address = first_block.address;

                    //  0  B0D   | CC                   | int3
                    //  1  B0E   | CC                   | int3
                    //  2  B0F   | CC                   | int3
                    //  3
                    //  4        ; comment line
                    //  5
                    //  6  B10   | 48 83 EC 38          |  sub rsp, 38h
                    //  7  B14   | 4C 8B 4C 24 38       |  mov r9, [rsp+38h]
                    //  8  B19   | 48 8D 05 48 99 00 00 |  lea rax, [141017468h]
                    //  9  B20   | 41 B8 8E 01 00 00    |  mov r8d, 18Eh

                    // find boundary of blocks to replace
                    if let Some(until) = block_iter.find(|block| {
                        block.address_range().end >= first_address + REPLACE_BYTES as u64
                    }) {
                        let mut new_dis =
                            crate::disassemble_range(binary_data, first_address - 30, &mut |dis| {
                                assert!(dis.len() < 1000);
                                dis.last().unwrap().address_block() != until.address_range().end
                            })
                            .unwrap();

                        assert_eq!(
                            new_dis.last().unwrap().address_block(),
                            until.address_range().end
                        );

                        drop(block_iter);

                        new_dis.pop();

                        let removed_lines = until.index_range().end;
                        added_lines += new_dis.len() - removed_lines;

                        self.disassembly.splice(..removed_lines, new_dis);
                    } else {
                        todo!("asdf1");
                    }
                } else {
                    todo!("asdf2");
                }
            }

            return added_lines;

            //if let Some(first) = self.disassembly.iter().find_map(|l| l.address()) {
            //    let new_dis =
            //        crate::disassemble_range(binary_data, last, &mut |dis| dis.len() < new_lines)
            //            .unwrap();
            //    let len = new_dis.len();
            //    self.disassembly.extend(new_dis);
            //    return len;
            //}
        }
        0
    }

    pub fn append_disassembly(&mut self, new_lines: usize) {
        if let Some(binary_data) = &self.binary_data {
            if let Some(last) = self.disassembly.iter().rev().find_map(|l| l.address_end()) {
                let new_dis =
                    crate::disassemble_range(binary_data, last, &mut |dis| dis.len() < new_lines)
                        .unwrap();
                self.disassembly.extend(new_dis);
            }
        }
    }

    pub fn navigate_back(&mut self) {
        if self.navigation_index > 0 {
            self.navigation_index -= 1;
            let entry = &self.navigation_stack[self.navigation_index];
            self.current_address = entry.address;
            *self.disassembly_state.offset_mut() = entry.scroll_offset;
            self.needs_refresh = true;
        }
        self.log.log(format!(
            "nav # {} stack {:x?}",
            self.navigation_index, self.navigation_stack
        ));
    }

    pub fn navigate_forward(&mut self) {
        if self.navigation_index < self.navigation_stack.len() - 1 {
            self.navigation_index += 1;
            let entry = &self.navigation_stack[self.navigation_index];
            self.current_address = entry.address;
            *self.disassembly_state.offset_mut() = entry.scroll_offset;
            self.needs_refresh = true;
        }
        self.log.log(format!(
            "nav # {} stack {:x?}",
            self.navigation_index, self.navigation_stack
        ));
    }

    fn recv_log(&mut self) {
        while let Ok(entry) = self.log_rx.try_recv() {
            self.log_entries.push_back(entry);
            if self.log_entries.len() > 1000 {
                self.log_entries.pop_front();
            }
            self.log_state.select(Some(self.log_entries.len() - 1));
        }
    }

    pub fn toggle_log(&mut self) {
        self.show_log = !self.show_log;
    }
}

struct DisBlock<'d> {
    /// index of first line in block
    index: usize,
    /// address of all lines in block
    address: u64,
    block: &'d [DisassemblyLine],
}
impl DisBlock<'_> {
    fn address_range(&self) -> std::ops::Range<u64> {
        self.address..self.block.iter().find_map(|l| l.address_end()).unwrap()
    }
    fn index_range(&self) -> std::ops::Range<usize> {
        self.index..self.index + self.block.len()
    }
}

/// iterate over blocks of disassembly
fn iter_blocks(disassembly: &[DisassemblyLine]) -> impl Iterator<Item = DisBlock<'_>> {
    let mut index = 0;
    disassembly
        .chunk_by(|a, b| a.address_block() == b.address_block())
        .map(move |block| {
            let r = DisBlock {
                index,
                address: block[0].address_block(),
                block,
            };
            index += block.len();
            r
        })
}

fn jump_to_top(state: &mut ListState) {
    *state.offset_mut() = 0;
    state.select(Some(0));
}

fn jump_to_bottom(state: &mut ListState, item_len: usize) {
    let last = item_len.saturating_sub(1);
    //*self.state.offset_mut() = last;
    state.select(Some(last));
}

pub fn run_app<'data, B: Backend>(
    terminal: &mut Terminal<B>,
    mut app: App<'data>,
    tick_rate: Duration,
) -> io::Result<App<'data>> {
    let mut last_tick = Instant::now();
    loop {
        if app.needs_refresh {
            if let (Some(height), Some(binary_data)) = (
                terminal.size().ok().map(|s| s.height as usize),
                &app.binary_data,
            ) {
                const FROM_TOP: usize = 15;

                let mut first_line = None;
                match crate::disassemble_range(binary_data, app.current_address - 50, &mut |dis| {
                    if first_line.is_none() {
                        first_line = dis
                            .iter()
                            .position(|l| l.address().is_some_and(|a| a > app.current_address))
                            .map(|l| l - 1)
                    }
                    if let Some(first_line) = first_line {
                        dis.len() + FROM_TOP < height + first_line
                    } else {
                        true
                    }
                }) {
                    Ok(disassembly) => {
                        let selected = first_line.unwrap_or(0);
                        app.set_disassembly(disassembly);
                        app.disassembly_state.select(Some(selected));
                        *app.disassembly_state.offset_mut() = selected.saturating_sub(FROM_TOP);
                    }
                    Err(e) => {
                        eprintln!("Error refreshing disassembly: {}", e);
                    }
                }
                app.needs_refresh = false;
            }
        }

        terminal.draw(|f| ui(f, &mut app))?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);

                    // Handle goto mode
                    if app.goto_mode && !ctrl {
                        match key.code {
                            KeyCode::Esc => app.toggle_goto_mode(),
                            KeyCode::Backspace => app.backspace_goto_query(),
                            KeyCode::Enter => app.submit_goto_query(),
                            KeyCode::Char(c) if c.is_ascii_hexdigit() || c == 'x' => {
                                app.add_to_goto_query(c)
                            }
                            _ => {}
                        }
                    } else if app.active_pane == Pane::XRefs && app.xref_mode && !ctrl {
                        match key.code {
                            KeyCode::Esc => app.toggle_xref_mode(),
                            KeyCode::Backspace => app.backspace_xref_query(),
                            KeyCode::Enter => app.submit_xref_query(),
                            KeyCode::Char(c) if c.is_ascii_hexdigit() || c == 'x' => {
                                app.add_to_xref_query(c)
                            }
                            _ => {}
                        }
                    } else if app.active_pane == Pane::Symbols && app.search_mode && !ctrl {
                        match key.code {
                            KeyCode::Esc => app.toggle_search(),
                            KeyCode::Backspace => app.backspace_search(),
                            KeyCode::Enter => app.select_symbol(),
                            KeyCode::Up => app.scroll_up(1),
                            KeyCode::Down => app.scroll_down(1),
                            KeyCode::Char(c) => app.add_to_search(c),
                            _ => {}
                        }
                    } else {
                        match key.code {
                            KeyCode::Char('q') => {
                                app.should_quit = true;
                                break;
                            }

                            KeyCode::Char('h') if ctrl => app.active_pane = Pane::Disassembly,
                            KeyCode::Char('l') if ctrl => app.active_pane = Pane::Symbols,
                            KeyCode::Char('g') if ctrl => {
                                app.toggle_goto_mode();
                            }

                            KeyCode::Char('?') => app.toggle_help(),
                            KeyCode::Tab => app.toggle_pane(),
                            KeyCode::Up | KeyCode::Char('k') => app.scroll_up(1),
                            KeyCode::Down | KeyCode::Char('j') => app.scroll_down(1),
                            KeyCode::PageUp => {
                                let height = terminal.size()?.height as usize;
                                app.scroll_up(height - 2);
                            }
                            KeyCode::PageDown => {
                                let height = terminal.size()?.height as usize;
                                app.scroll_down(height - 2);
                            }
                            KeyCode::Char('d') => {
                                let height = terminal.size()?.height as usize;
                                app.scroll_down((height - 2) / 2); // Half page down
                            }
                            KeyCode::Char('u') => {
                                let height = terminal.size()?.height as usize;
                                app.scroll_up((height - 2) / 2); // Half page up
                            }
                            KeyCode::Char('g') => app.jump_to_top(),
                            KeyCode::Char('G') => app.jump_to_bottom(),
                            KeyCode::Char('h') if !ctrl => app.navigate_back(),
                            KeyCode::Char('l') if !ctrl => app.navigate_forward(),
                            KeyCode::Char('L') => app.toggle_log(),

                            KeyCode::Char('/') => app.toggle_search(),
                            KeyCode::Char('x') => {
                                if app.active_pane == Pane::XRefs {
                                    app.toggle_xref_mode();
                                }
                            }
                            KeyCode::Enter => {
                                if app.active_pane == Pane::Symbols {
                                    app.select_symbol();
                                } else if app.active_pane == Pane::XRefs {
                                    app.select_xref();
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }

    Ok(app)
}

fn ui(f: &mut Frame, app: &mut App) {
    let chunks = if app.show_log {
        Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(80), Constraint::Percentage(20)].as_ref())
            .split(f.area())
    } else {
        Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(100)].as_ref())
            .split(f.area())
    };

    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(70), Constraint::Percentage(30)].as_ref())
        .split(chunks[0]);

    let chunks_vert = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(main_chunks[1]);

    let disassembly_items: Vec<ListItem> = app
        .disassembly
        .iter()
        .map(|line| match line {
            DisassemblyLine::Empty { .. } => ListItem::new(""),
            DisassemblyLine::Symbol { symbol, .. } => {
                let style = match symbol.kind {
                    SymbolKind::Function => Style::default().fg(Color::Yellow),
                    SymbolKind::Data => Style::default().fg(Color::Cyan),
                };

                ListItem::new(Line::from(vec![
                    Span::styled(" ; ", Style::default().fg(Color::DarkGray)),
                    Span::styled(symbol.display_name(), style),
                ]))
            }
            DisassemblyLine::Text { text, .. } => ListItem::new(Line::from(vec![Span::styled(
                text,
                Style::default().fg(Color::White),
            )])),
            DisassemblyLine::Instruction {
                address,
                bytes,
                instruction,
                comments,
            } => {
                let mut spans = vec![Span::styled(
                    format!("{:016X} ", address),
                    Style::default().fg(Color::White),
                )];

                for b in bytes {
                    spans.push(Span::styled(
                        format!("{:02X} ", b),
                        Style::default().fg(Color::DarkGray),
                    ));
                }

                for _ in bytes.len()..12 {
                    spans.push(Span::raw("   "));
                }

                let instr_parts: Vec<&str> = instruction.split_whitespace().collect();
                if !instr_parts.is_empty() {
                    spans.push(Span::styled(
                        instr_parts[0],
                        Style::default().fg(Color::Red),
                    ));
                    spans.push(Span::raw(" "));

                    if instr_parts.len() > 1 {
                        let operands = instr_parts[1..].join(" ");
                        let operand_spans = highlight_operands(&operands);
                        spans.extend(operand_spans);
                    }
                } else {
                    spans.push(Span::raw(instruction));
                }

                if !comments.is_empty() {
                    spans.push(Span::styled(" ; ", Style::default().fg(Color::DarkGray)));

                    for (i, comment) in comments.iter().enumerate() {
                        if i > 0 {
                            spans.push(Span::raw(" "));
                        }

                        match comment {
                            DisassemblyComment::BranchTarget(sym) => {
                                spans.push(Span::styled(
                                    "-> ",
                                    Style::default().fg(Color::DarkGray),
                                ));
                                spans.push(Span::styled(
                                    sym.display_name(),
                                    Style::default().fg(Color::Yellow),
                                ));
                            }
                            DisassemblyComment::MemoryReference(sym) => {
                                spans.push(Span::styled(
                                    "ref ",
                                    Style::default().fg(Color::DarkGray),
                                ));
                                spans.push(Span::styled(
                                    sym.display_name(),
                                    Style::default().fg(Color::Yellow),
                                ));
                            }
                            DisassemblyComment::Data(data) => {
                                spans.push(format_data_spans(data));
                            }
                        }
                    }
                }

                ListItem::new(Line::from(spans))
            }
        })
        .collect();

    let disassembly_list = List::new(disassembly_items)
        .block(
            Block::default()
                .title(format!(
                    "Disassembly @ 0x{:X} ({} lines)",
                    app.current_address,
                    app.disassembly.len()
                ))
                .borders(Borders::ALL)
                .border_style(if app.active_pane == Pane::Disassembly {
                    Style::default().fg(Color::Yellow)
                } else {
                    Style::default()
                }),
        )
        .highlight_style(Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED));

    f.render_stateful_widget(disassembly_list, main_chunks[0], &mut app.disassembly_state);

    // Symbols pane with integrated search
    {
        let symbols_area = chunks_vert[0];

        let search_snapshot = app.search_mode.then(|| app.nucleo.snapshot());

        let symbols_block = Block::default()
            .title(if let Some(search) = &search_snapshot {
                format!("Symbols ({} matches)", search.matched_item_count())
            } else {
                format!("Symbols ({} total)", app.symbols.len())
            })
            .borders(Borders::ALL)
            .border_style(if app.active_pane == Pane::Symbols {
                Style::default().fg(Color::Yellow)
            } else {
                Style::default()
            });

        // Split the inner area for search bar and symbol list
        let inner_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Search bar takes just one line
                Constraint::Min(1),    // Symbol list takes the rest
            ])
            .split(symbols_block.inner(symbols_area));

        // Always render the search bar, but style it differently when not in search mode
        let search_text = if app.search_mode {
            format!("/{}", app.search_query)
        } else {
            "Press '/' to search".to_string()
        };

        let search_style = if app.search_mode {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let search_bar = Paragraph::new(search_text).style(search_style);

        struct SymListProd<'a>(&'a [SymbolInfo]);
        impl<'a> ListItemProducer<'a> for SymListProd<'a> {
            fn total(&self) -> usize {
                self.0.len()
            }
            fn items(
                &self,
                range: std::ops::Range<usize>,
            ) -> impl ExactSizeIterator<Item = ListItem<'a>> {
                self.0[range].iter().map(|symbol| {
                    let style = match symbol.kind {
                        SymbolKind::Function => Style::default().fg(Color::Yellow),
                        SymbolKind::Data => Style::default().fg(Color::Cyan),
                    };

                    ListItem::new(Line::from(vec![
                        Span::styled(
                            format!("{:X}: ", symbol.address),
                            Style::default().fg(Color::White),
                        ),
                        Span::styled(symbol.display_name(), style),
                    ]))
                })
            }
        }

        struct SymSearchListProd<'a>(&'a nucleo::Snapshot<SymbolInfo>, &'a str);
        impl<'a> ListItemProducer<'a> for SymSearchListProd<'a> {
            fn total(&self) -> usize {
                self.0.matched_item_count() as usize
            }
            fn items(
                &self,
                range: std::ops::Range<usize>,
            ) -> impl ExactSizeIterator<Item = ListItem<'a>> {
                let query = &self.1;
                self.0
                    .matched_items(range.start as u32..range.end as u32)
                    .map(move |item| {
                        let symbol = item.data;
                        let name = symbol.display_name();

                        // Highlight matching parts if there's a search query
                        if query.is_empty() {
                            ListItem::new(Line::from(vec![
                                Span::styled(
                                    format!("{:X}: ", symbol.address),
                                    Style::default().fg(Color::White),
                                ),
                                symbol.display_name().into(),
                            ]))
                        } else {
                            // Create a styled line with highlighted matches
                            let mut spans = vec![Span::styled(
                                format!("{:X}: ", symbol.address),
                                Style::default().fg(Color::White),
                            )];

                            // Use the highlight_matches function to get match positions
                            let highlights = highlight_matches(name, query);

                            for (start, end, is_match) in highlights {
                                let segment = &name[start..end];
                                if is_match {
                                    spans.push(Span::styled(
                                        segment,
                                        Style::default()
                                            .fg(Color::Yellow)
                                            .add_modifier(Modifier::BOLD),
                                    ));
                                } else {
                                    spans.push(Span::raw(segment));
                                }
                            }
                            ListItem::new(Line::from(spans))
                        }
                    })
            }
        }

        f.render_widget(&symbols_block, symbols_area);
        f.render_widget(search_bar, inner_chunks[0]);

        if let Some(search) = search_snapshot {
            let list = LazyList::new(SymSearchListProd(search, &app.search_query));
            f.render_stateful_widget(list, inner_chunks[1], &mut app.symbol_list_state);
        } else {
            let list = LazyList::new(SymListProd(&app.symbols));
            f.render_stateful_widget(list, inner_chunks[1], &mut app.symbol_list_state);
        }
    }

    // XRefs pane (bottom right)
    let xrefs_area = chunks_vert[1];

    // Calculate the inner area of the XRefs block
    let xrefs_inner_area = Block::default()
        .title(format!("XRefs ({} found)", app.xrefs.len()))
        .borders(Borders::ALL)
        .border_style(if app.active_pane == Pane::XRefs {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default()
        })
        .inner(xrefs_area);

    // Render the block first
    f.render_widget(
        Block::default()
            .title(format!("XRefs ({} found)", app.xrefs.len()))
            .borders(Borders::ALL)
            .border_style(if app.active_pane == Pane::XRefs {
                Style::default().fg(Color::Yellow)
            } else {
                Style::default()
            }),
        xrefs_area,
    );

    // Split the inner area for query bar and XRefs list
    let xrefs_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1), // Query bar takes just one line
            Constraint::Min(1),    // XRefs list takes the rest
        ])
        .split(xrefs_inner_area);

    // Always render the query bar, but style it differently when not in XRef mode
    let query_text = if app.xref_mode {
        format!("Address: {}", app.xref_query)
    } else {
        "Press 'x' to enter address".to_string()
    };

    let query_style = if app.xref_mode {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let query_bar = Paragraph::new(query_text).style(query_style);

    f.render_widget(query_bar, xrefs_chunks[0]);

    struct XrefListProd<'a>(&'a [XRefInfo]);
    impl<'a> ListItemProducer<'a> for XrefListProd<'a> {
        fn total(&self) -> usize {
            self.0.len()
        }
        fn items(
            &self,
            range: std::ops::Range<usize>,
        ) -> impl ExactSizeIterator<Item = ListItem<'a>> {
            self.0[range].iter().map(|xref| {
                let kind_str = match xref.kind {
                    XRefKind::Call => "call",
                    XRefKind::Jump => "jmp",
                    XRefKind::DataRef => "ref",
                };
                ListItem::new(Line::from(vec![Span::styled(
                    format!(
                        "{:016X} {} {:016X}",
                        xref.from_address, kind_str, xref.to_address
                    ),
                    Style::default().fg(Color::White),
                )]))
            })
        }
    }

    // Render the xrefs list
    f.render_stateful_widget(
        LazyList::new(XrefListProd(&app.xrefs)),
        xrefs_chunks[1],
        &mut app.xref_list_state,
    );

    // If in goto mode, show the goto dialog
    if app.goto_mode {
        // Create a centered dialog box
        let dialog_area = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(40),
                    Constraint::Length(3),
                    Constraint::Percentage(40),
                ]
                .as_ref(),
            )
            .split(f.area())[1];

        let dialog_area = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(
                [
                    Constraint::Percentage(30),
                    Constraint::Percentage(40),
                    Constraint::Percentage(30),
                ]
                .as_ref(),
            )
            .split(dialog_area)[1];

        // Create the goto input box
        let goto_input = Paragraph::new(app.goto_query.to_string())
            .block(
                Block::default()
                    .title("Go to address (hex)")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Yellow)),
            )
            .style(Style::default().fg(Color::Yellow));

        // Render the dialog
        f.render_widget(goto_input, dialog_area);
    }

    if app.show_help {
        let help_text = vec![
            "Help:",
            "q - Quit",
            "h/? - Toggle help",
            "Tab - Switch pane",
            "↑/↓/j/k - Scroll up/down",
            "PgUp/PgDn - Page up/down",
            "u/d - Half page up/down",
            "g/G - Jump to top/bottom",
            "h/l - Navigate back/forward",
            "L - Toggle debug log",
            "/ - Search symbols",
            "x - Find XRefs",
            "Ctrl+G - Go to address",
            "Enter - Select item",
            "",
            "In search/XRef/goto mode:",
            "Esc - Exit mode",
            "Backspace - Delete character",
        ];

        let help_paragraph = Paragraph::new(Text::from(help_text.join("\n")))
            .block(Block::default().title("Help").borders(Borders::ALL))
            .style(Style::default().fg(Color::White).bg(Color::Black));

        let help_area = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(30),
                    Constraint::Percentage(40),
                    Constraint::Percentage(30),
                ]
                .as_ref(),
            )
            .split(f.area())[1];

        let help_area = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(
                [
                    Constraint::Percentage(20),
                    Constraint::Percentage(60),
                    Constraint::Percentage(20),
                ]
                .as_ref(),
            )
            .split(help_area)[1];

        f.render_widget(help_paragraph, help_area);
    }

    // Add log panel at the bottom if enabled
    if app.show_log {
        app.recv_log();

        let log_items: Vec<ListItem> = app
            .log_entries
            .iter()
            .map(|entry| {
                let elapsed = entry.timestamp.elapsed();
                let time_str = format!(
                    "{:02}:{:02}:{:02}",
                    elapsed.as_secs() / 3600,
                    (elapsed.as_secs() % 3600) / 60,
                    elapsed.as_secs() % 60
                );
                ListItem::new(format!("[{}] {}", time_str, entry.message))
            })
            .collect();

        let log_list = List::new(log_items)
            .block(
                Block::default()
                    .title("Debug Log")
                    .borders(Borders::ALL)
                    .border_style(if app.active_pane == Pane::DebugLog {
                        Style::default().fg(Color::Yellow)
                    } else {
                        Style::default()
                    }),
            )
            .highlight_style(Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED));

        f.render_stateful_widget(log_list, chunks[1], &mut app.log_state);
    }
}

fn highlight_operands(operands: &str) -> Vec<Span<'static>> {
    let mut spans = Vec::new();

    for part in operands.split(',') {
        if !spans.is_empty() {
            spans.push(Span::styled(",", Style::default().fg(Color::White)));
            spans.push(Span::raw(" "));
        }

        let part = part.trim().to_string();

        if part.starts_with("r")
            || part.starts_with("e")
            || [
                "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp", "eax", "ebx", "ecx", "edx",
                "esi", "edi", "ebp", "esp",
            ]
            .contains(&part.as_str())
        {
            spans.push(Span::styled(part, Style::default().fg(Color::Blue)));
        } else if part.starts_with("0x") || part.chars().next().is_some_and(|c| c.is_ascii_digit())
        {
            spans.push(Span::styled(part, Style::default().fg(Color::Cyan)));
        } else {
            spans.push(Span::raw(part));
        }
    }

    spans
}

fn format_data_spans(data: &[u8]) -> Span {
    enum StrType {
        Str,
        WStr,
    }

    let str_data = {
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

            Some((StrType::WStr, String::from_utf16_lossy(&chars)))
        } else {
            let len = data
                .iter()
                .position(|c| *c == 0 || !char::from(*c).is_ascii())
                .unwrap_or(data.len());

            if len > 5 {
                Some((
                    StrType::Str,
                    String::from_utf8_lossy(&data[..len]).to_string(),
                ))
            } else {
                None
            }
        }
    };

    if let Some((t, str)) = str_data {
        let prefix = match t {
            StrType::Str => "",
            StrType::WStr => "L",
        };
        Span::styled(
            format!("{}{:?}", prefix, str),
            Style::default().fg(Color::Red),
        )
    } else {
        let mut output = String::from("[");
        for b in data {
            output.push_str(&format!(" {b:02X}"));
        }
        output.push(']');
        Span::styled(output, Style::default().fg(Color::Cyan))
    }
}

pub fn setup_terminal() -> Result<Terminal<CrosstermBackend<io::Stdout>>, Box<dyn Error>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let terminal = Terminal::new(backend)?;
    Ok(terminal)
}

pub fn restore_terminal(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
) -> Result<(), Box<dyn Error>> {
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;
    Ok(())
}
