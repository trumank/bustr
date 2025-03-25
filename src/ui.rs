use crate::{
    BinaryData, DisassemblyLine, SymbolInfo, SymbolKind,
    disassembly_ui::{DisassemblyState, DisassemblyWidget},
    fuzzy::highlight_matches,
    lazy_list::{LazyList, ListItemProducer},
    xref_ui::{XRefState, XRefWidget},
};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind, KeyModifiers,
    },
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use nucleo::pattern::{CaseMatching, Normalization};
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
pub struct NavigationEntry {
    pub address: u64,
    pub scroll_offset: usize,
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
    pub symbols: Vec<SymbolInfo>,
    pub show_help: bool,
    pub should_quit: bool,
    pub show_log: bool,

    pub active_pane: Pane,
    pub search_mode: bool,

    pub disassembly_state: DisassemblyState,
    pub symbol_list_state: ListState,
    pub file_path: Option<PathBuf>,
    pub pdb_path: Option<PathBuf>,
    pub binary_data: Option<BinaryData<'data>>,
    pub search_query: String,
    nucleo: nucleo::Nucleo<SymbolInfo>,

    pub goto_mode: bool,
    pub goto_query: String,

    log_entries: VecDeque<LogEntry>,
    log_state: ListState,
    log: Log,
    log_rx: mpsc::Receiver<LogEntry>,
    pub xref_state: XRefState,
}

#[derive(PartialEq)]
pub enum Pane {
    Disassembly,
    Symbols,
    XRefs,
    DebugLog,
}

impl<'data> App<'data> {
    pub fn new() -> Self {
        let mut disassembly_state = ListState::default();
        disassembly_state.select(Some(0));

        let (log_tx, log_rx) = mpsc::channel();
        let log = Log(log_tx);

        Self {
            symbols: Vec::new(),
            show_help: false,
            should_quit: false,
            show_log: true,

            active_pane: Pane::Disassembly,
            search_mode: false,

            disassembly_state: DisassemblyState::new(),
            symbol_list_state: ListState::default(),
            file_path: None,
            pdb_path: None,
            binary_data: None,
            search_query: String::new(),
            nucleo: nucleo::Nucleo::new(
                nucleo::Config::DEFAULT,
                std::sync::Arc::new(|| { /* erm */ }),
                None,
                1,
            ),
            goto_mode: false,
            goto_query: String::new(),

            log_entries: VecDeque::with_capacity(1000),
            log_state: ListState::default(),
            log,
            log_rx,
            xref_state: XRefState::new(),
        }
    }

    pub fn set_disassembly(&mut self, disassembly: Vec<DisassemblyLine>) {
        self.disassembly_state.disassembly = disassembly;
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

    pub fn set_binary_data(&mut self, binary_data: BinaryData<'data>) {
        self.binary_data = Some(binary_data);
    }

    pub fn scroll_up(&mut self, amount: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                if let Some(binary_data) = &self.binary_data {
                    self.disassembly_state.scroll_up(binary_data, amount);
                }
            }
            Pane::Symbols => {
                self.symbol_list_state.scroll_up_by(amount as u16);
                self.select_symbol();
            }
            Pane::XRefs => {
                self.xref_state.scroll_up(amount);
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
                if let Some(binary_data) = &self.binary_data {
                    self.disassembly_state.scroll_down(binary_data, amount);
                }
            }
            Pane::Symbols => {
                self.symbol_list_state.scroll_down_by(amount as u16);
                self.select_symbol();
            }
            Pane::XRefs => {
                self.xref_state.scroll_down(amount);
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
                        self.disassembly_state
                            .set_current_address(item.data.address);
                    }
                } else if idx < self.symbols.len() {
                    //self.selected_symbol_index = Some(idx);
                    let symbol = &self.symbols[idx];
                    self.disassembly_state.set_current_address(symbol.address);
                }
            }
        }
    }

    // Method to jump to the top of the current pane
    pub fn jump_to_top(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                if let Some(first_addr) = self.find_first_address() {
                    self.disassembly_state.set_current_address(first_addr);
                }
                *self.disassembly_state.offset_mut() = 0;
                self.disassembly_state.select(Some(0));
            }
            Pane::Symbols => {
                jump_to_top(&mut self.symbol_list_state);
                self.select_symbol();
            }
            Pane::XRefs => {
                self.xref_state.jump_to_top();
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
                if let Some(last_addr) = self.find_last_address() {
                    self.disassembly_state.set_current_address(last_addr);
                }
            }
            Pane::Symbols => {
                // TODO handle search
                jump_to_bottom(&mut self.symbol_list_state, self.symbols.len());
                self.select_symbol();
            }
            Pane::XRefs => {
                self.xref_state.jump_to_bottom();
                self.select_xref();
            }
            Pane::DebugLog => {
                if !self.log_entries.is_empty() {
                    self.log_state.select(Some(self.log_entries.len() - 1));
                }
            }
        }
    }

    fn find_first_address(&self) -> Option<u64> {
        self.binary_data
            .as_ref()
            .and_then(|b| b.file.memory.sections().next())
            .map(|s| s.address() as u64)
    }

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

    pub fn find_xref(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                if let Some(address) = self
                    .disassembly_state
                    .selected()
                    .and_then(|s| self.disassembly_state.disassembly.get(s))
                    .and_then(|l| l.address())
                {
                    self.find_xrefs(address);
                    self.active_pane = Pane::XRefs;
                }
            }
            Pane::Symbols => {
                if let Some(symbol) = self
                    .symbol_list_state
                    .selected()
                    .and_then(|s| self.symbols.get(s))
                {
                    self.find_xrefs(symbol.address);
                    self.active_pane = Pane::XRefs;
                }
            }
            Pane::XRefs => {
                self.xref_state.toggle_search_mode();
            }
            _ => {}
        }
    }

    pub fn add_to_xref_query(&mut self, c: char) {
        self.xref_state.add_to_query(c);
    }

    pub fn remove_from_xref_query(&mut self) {
        self.xref_state.remove_from_query();
    }

    pub fn find_xrefs(&mut self, address: u64) {
        if let Some(binary_data) = &self.binary_data {
            self.xref_state.find_xrefs(binary_data, address);
        }
    }

    pub fn select_xref(&mut self) {
        if self.active_pane == Pane::XRefs {
            if let Some(xref) = self.xref_state.selected_xref() {
                self.disassembly_state
                    .set_current_address(xref.from_address);
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
                self.disassembly_state.set_current_address(addr);
                self.active_pane = Pane::Disassembly;
                self.goto_mode = false;
            }
        }
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

    pub fn follow_address_reference(&mut self) {
        if self.active_pane == Pane::Disassembly {
            self.disassembly_state.follow_address_reference();
        }
    }
}

pub fn jump_to_top(state: &mut ListState) {
    *state.offset_mut() = 0;
    state.select(Some(0));
}

pub fn jump_to_bottom(state: &mut ListState, item_len: usize) {
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
        if app.disassembly_state.needs_refresh {
            if let (Some(height), Some(binary_data)) = (
                terminal.size().ok().map(|s| s.height as usize),
                &app.binary_data,
            ) {
                const FROM_TOP: usize = 15;

                let mut first_line = None;
                match crate::disassemble_range(
                    binary_data,
                    app.disassembly_state.current_address - 50,
                    &mut |dis| {
                        if first_line.is_none() {
                            first_line = dis
                                .iter()
                                .position(|l| {
                                    l.address()
                                        .is_some_and(|a| a > app.disassembly_state.current_address)
                                })
                                .map(|l| l - 1)
                        }
                        if let Some(first_line) = first_line {
                            dis.len() + FROM_TOP < height + first_line
                        } else {
                            true
                        }
                    },
                ) {
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
                app.disassembly_state.needs_refresh = false;
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
                    } else if app.active_pane == Pane::XRefs && app.xref_state.search_mode && !ctrl
                    {
                        match key.code {
                            KeyCode::Esc => app.find_xref(),
                            KeyCode::Backspace => app.remove_from_xref_query(),
                            KeyCode::Enter => {
                                app.find_xrefs(
                                    u64::from_str_radix(&app.xref_state.query, 16).unwrap(),
                                );
                                app.xref_state.toggle_search_mode();
                            }
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
                            KeyCode::Tab => app.toggle_pane(),
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
                            KeyCode::Char('h') if !ctrl => app.disassembly_state.navigate_back(),
                            KeyCode::Char('l') if !ctrl => app.disassembly_state.navigate_forward(),
                            KeyCode::Char('L') => app.toggle_log(),

                            KeyCode::Char('/') => app.toggle_search(),
                            KeyCode::Char('x') => {
                                app.find_xref();
                            }
                            KeyCode::Enter => {
                                if app.active_pane == Pane::Symbols {
                                    app.select_symbol();
                                } else if app.active_pane == Pane::XRefs {
                                    app.select_xref();
                                }
                            }
                            KeyCode::Char('f') => app.follow_address_reference(),
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

    let disassembly_widget = DisassemblyWidget::new()
        .block(
            Block::default()
                .title(format!(
                    "Disassembly @ 0x{:X} ({} lines)",
                    app.disassembly_state.current_address(),
                    app.disassembly_state.disassembly.len()
                ))
                .borders(Borders::ALL)
                .border_style(if app.active_pane == Pane::Disassembly {
                    Style::default().fg(Color::Yellow)
                } else {
                    Style::default()
                }),
        )
        .highlight_style(Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED));

    f.render_stateful_widget(
        disassembly_widget,
        main_chunks[0],
        &mut app.disassembly_state,
    );

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
        .title(format!("XRefs ({} found)", app.xref_state.xrefs.len()))
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
            .title(format!("XRefs ({} found)", app.xref_state.xrefs.len()))
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
    let query_text = if app.xref_state.search_mode {
        format!("Address: {}", app.xref_state.query)
    } else {
        "Press 'x' to enter address".to_string()
    };

    let query_style = if app.xref_state.search_mode {
        Style::default().fg(Color::Yellow)
    } else {
        Style::default().fg(Color::DarkGray)
    };

    let query_bar = Paragraph::new(query_text).style(query_style);

    f.render_widget(query_bar, xrefs_chunks[0]);

    // Create the XRef widget
    let xref_widget = XRefWidget::new()
        .block(
            Block::default()
                .title(format!(
                    "XRefs to 0x{:X} ({} found)",
                    app.xref_state.current_address().unwrap_or(0),
                    app.xref_state.xrefs.len()
                ))
                .borders(Borders::ALL)
                .border_style(if app.active_pane == Pane::XRefs {
                    Style::default().fg(Color::Yellow)
                } else {
                    Style::default()
                }),
        )
        .highlight_style(Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED));

    // Render the XRef widget
    f.render_stateful_widget(xref_widget, xrefs_chunks[1], &mut app.xref_state);

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
            "f - Follow address reference",
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
