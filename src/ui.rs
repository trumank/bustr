use crate::{DisassemblerError, DisassemblyComment, DisassemblyLine, SymbolInfo, SymbolKind};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    Frame, Terminal,
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph},
};
use std::{
    error::Error,
    io,
    path::Path,
    path::PathBuf,
    time::{Duration, Instant},
};

pub struct App {
    pub disassembly: Vec<DisassemblyLine>,
    pub symbols: Vec<SymbolInfo>,
    pub current_scroll: usize,
    pub symbol_scroll: usize,
    pub selected_symbol_index: Option<usize>,
    pub show_help: bool,
    pub should_quit: bool,
    pub active_pane: Pane,
    pub disassembly_state: ListState,
    pub symbol_state: ListState,
    pub current_address: u64,
    pub file_path: Option<PathBuf>,
    pub pdb_path: Option<PathBuf>,
    pub needs_refresh: bool,
}

#[derive(PartialEq)]
pub enum Pane {
    Disassembly,
    Symbols,
}

impl App {
    pub fn new() -> Self {
        let mut disassembly_state = ListState::default();
        disassembly_state.select(Some(0));

        let mut symbol_state = ListState::default();
        symbol_state.select(Some(0));

        Self {
            disassembly: Vec::new(),
            symbols: Vec::new(),
            current_scroll: 0,
            symbol_scroll: 0,
            selected_symbol_index: None,
            show_help: false,
            should_quit: false,
            active_pane: Pane::Disassembly,
            disassembly_state,
            symbol_state,
            current_address: 0,
            file_path: None,
            pdb_path: None,
            needs_refresh: false,
        }
    }

    pub fn set_disassembly(&mut self, disassembly: Vec<DisassemblyLine>) {
        self.disassembly = disassembly;
    }

    pub fn set_symbols(&mut self, symbols: Vec<SymbolInfo>) {
        self.symbols = symbols;
    }

    pub fn set_file_info(&mut self, file_path: &Path, pdb_path: Option<&Path>) {
        self.file_path = Some(file_path.to_path_buf());
        self.pdb_path = pdb_path.map(|p| p.to_path_buf());
    }

    pub fn set_current_address(&mut self, address: u64) {
        self.current_address = address;
        self.needs_refresh = true;
    }

    pub fn scroll_up(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                if self.current_scroll > 0 {
                    self.current_scroll -= 1;
                    self.disassembly_state.select(Some(self.current_scroll));
                } else if !self.disassembly.is_empty() {
                    if let Some(prev_addr) = self.find_previous_address() {
                        self.set_current_address(prev_addr);
                    }
                }
            }
            Pane::Symbols => {
                if self.symbol_scroll > 0 {
                    self.symbol_scroll -= 1;
                    self.symbol_state.select(Some(self.symbol_scroll));
                }
            }
        }
    }

    pub fn scroll_down(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                if self.current_scroll < self.disassembly.len().saturating_sub(1) {
                    self.current_scroll += 1;
                    self.disassembly_state.select(Some(self.current_scroll));
                } else if !self.disassembly.is_empty() {
                    if let Some(next_addr) = self.find_next_address() {
                        self.set_current_address(next_addr);
                    }
                }
            }
            Pane::Symbols => {
                if self.symbol_scroll < self.symbols.len().saturating_sub(1) {
                    self.symbol_scroll += 1;
                    self.symbol_state.select(Some(self.symbol_scroll));
                }
            }
        }
    }

    pub fn page_up(&mut self, page_size: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                if self.current_scroll > page_size {
                    self.current_scroll -= page_size;
                    self.disassembly_state.select(Some(self.current_scroll));
                } else {
                    self.current_scroll = 0;
                    self.disassembly_state.select(Some(0));

                    if !self.disassembly.is_empty() {
                        self.set_current_address(
                            self.current_address.saturating_sub(page_size as u64 * 16),
                        );
                    }
                }
            }
            Pane::Symbols => {
                self.symbol_scroll = self.symbol_scroll.saturating_sub(page_size);
                self.symbol_state.select(Some(self.symbol_scroll));
            }
        }
    }

    pub fn page_down(&mut self, page_size: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                let max = self.disassembly.len().saturating_sub(1);
                if self.current_scroll + page_size < max {
                    self.current_scroll += page_size;
                    self.disassembly_state.select(Some(self.current_scroll));
                } else {
                    self.current_scroll = max;
                    self.disassembly_state.select(Some(max));

                    if !self.disassembly.is_empty() {
                        self.set_current_address(self.current_address + page_size as u64 * 16);
                    }
                }
            }
            Pane::Symbols => {
                let max = self.symbols.len().saturating_sub(1);
                self.symbol_scroll = (self.symbol_scroll + page_size).min(max);
                self.symbol_state.select(Some(self.symbol_scroll));
            }
        }
    }

    pub fn toggle_help(&mut self) {
        self.show_help = !self.show_help;
    }

    pub fn toggle_pane(&mut self) {
        self.active_pane = match self.active_pane {
            Pane::Disassembly => Pane::Symbols,
            Pane::Symbols => Pane::Disassembly,
        };
    }

    pub fn select_symbol(&mut self) {
        if self.active_pane == Pane::Symbols && !self.symbols.is_empty() {
            let idx = self.symbol_scroll;
            if idx < self.symbols.len() {
                self.selected_symbol_index = Some(idx);

                let symbol = &self.symbols[idx];
                self.set_current_address(symbol.address);

                self.active_pane = Pane::Disassembly;
            }
        }
    }

    fn find_previous_address(&self) -> Option<u64> {
        if let Some(first_instr) = self.disassembly.iter().find_map(|line| {
            if let DisassemblyLine::Instruction { address, .. } = line {
                Some(address)
            } else {
                None
            }
        }) {
            Some(first_instr.saturating_sub(4))
        } else {
            None
        }
    }

    fn find_next_address(&self) -> Option<u64> {
        if let Some(last_instr) = self.disassembly.iter().rev().find_map(|line| {
            if let DisassemblyLine::Instruction { address, bytes, .. } = line {
                Some((*address, bytes.len() as u64))
            } else {
                None
            }
        }) {
            Some(last_instr.0 + last_instr.1)
        } else {
            None
        }
    }
}

pub fn run_app<B: Backend>(
    terminal: &mut Terminal<B>,
    mut app: App,
    tick_rate: Duration,
) -> io::Result<App> {
    let mut last_tick = Instant::now();
    loop {
        if app.needs_refresh {
            if let (Some(file_path), Some(height)) = (
                &app.file_path,
                terminal.size().ok().map(|s| s.height as usize),
            ) {
                match crate::disassemble_range(
                    file_path,
                    app.pdb_path.as_deref(),
                    app.current_address,
                    height,
                ) {
                    Ok((disassembly, _)) => {
                        app.set_disassembly(disassembly);
                        app.current_scroll = 0;
                        app.disassembly_state.select(Some(0));
                    }
                    Err(e) => {
                        eprintln!("Error refreshing disassembly: {}", e);
                    }
                }
                app.needs_refresh = false;
            }
        }

        terminal.draw(|f| ui(f, &app))?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    match key.code {
                        KeyCode::Char('q') => {
                            app.should_quit = true;
                            break;
                        }
                        KeyCode::Char('?') | KeyCode::Char('h') => app.toggle_help(),
                        KeyCode::Tab => app.toggle_pane(),
                        KeyCode::Up => app.scroll_up(),
                        KeyCode::Down => app.scroll_down(),
                        KeyCode::PageUp => app.page_up(10),
                        KeyCode::PageDown => app.page_down(10),
                        KeyCode::Enter => app.select_symbol(),
                        _ => {}
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

fn ui(f: &mut Frame, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(70), Constraint::Percentage(30)].as_ref())
        .split(f.size());

    let disassembly_items: Vec<ListItem> = app
        .disassembly
        .iter()
        .map(|line| match line {
            DisassemblyLine::Empty => ListItem::new(""),
            DisassemblyLine::Symbol(symbol) => {
                let name = if let Some(n) = &symbol.demangled {
                    n
                } else {
                    &symbol.name
                };

                let style = match symbol.kind {
                    SymbolKind::Function => Style::default().fg(Color::Yellow),
                    SymbolKind::Data => Style::default().fg(Color::Cyan),
                };

                ListItem::new(Line::from(vec![
                    Span::styled(" ; ", Style::default().fg(Color::DarkGray)),
                    Span::styled(name, style),
                ]))
            }
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
                                let name = if let Some(n) = &sym.demangled {
                                    n
                                } else {
                                    &sym.name
                                };
                                spans.push(Span::styled(name, Style::default().fg(Color::Yellow)));
                            }
                            DisassemblyComment::MemoryReference(sym) => {
                                spans.push(Span::styled(
                                    "ref ",
                                    Style::default().fg(Color::DarkGray),
                                ));
                                let name = if let Some(n) = &sym.demangled {
                                    n
                                } else {
                                    &sym.name
                                };
                                spans.push(Span::styled(name, Style::default().fg(Color::Yellow)));
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
        .highlight_style(Style::default().add_modifier(Modifier::BOLD));

    f.render_stateful_widget(
        disassembly_list,
        chunks[0],
        &mut app.disassembly_state.clone(),
    );

    let symbols_height = chunks[1].height as usize - 2;
    let start_idx = app.symbol_scroll.saturating_sub(symbols_height / 2);
    let end_idx = (start_idx + symbols_height).min(app.symbols.len());

    let symbol_items: Vec<ListItem> = app.symbols[start_idx..end_idx]
        .iter()
        .map(|symbol| {
            let name = if let Some(n) = &symbol.demangled {
                n
            } else {
                &symbol.name
            };
            ListItem::new(format!("{:X}: {}", symbol.address, name))
        })
        .collect();

    let symbols_list = List::new(symbol_items)
        .block(
            Block::default()
                .title(format!("Symbols ({} total)", app.symbols.len()))
                .borders(Borders::ALL)
                .border_style(if app.active_pane == Pane::Symbols {
                    Style::default().fg(Color::Yellow)
                } else {
                    Style::default()
                }),
        )
        .highlight_style(Style::default().add_modifier(Modifier::BOLD));

    let mut symbol_state = app.symbol_state.clone();
    if app.symbol_scroll >= start_idx && app.symbol_scroll < end_idx {
        symbol_state.select(Some(app.symbol_scroll - start_idx));
    }

    f.render_stateful_widget(symbols_list, chunks[1], &mut symbol_state);

    if app.show_help {
        let help_text = vec![
            "Help:",
            "q - Quit",
            "h/? - Toggle help",
            "Tab - Switch pane",
            "↑/↓ - Scroll up/down",
            "PgUp/PgDn - Page up/down",
            "Enter - Select symbol (in symbol pane)",
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
            .split(f.size())[1];

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
        } else if part.starts_with("0x") || part.chars().next().map_or(false, |c| c.is_digit(10)) {
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
