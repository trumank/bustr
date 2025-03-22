use crate::{DisassemblerError, SymbolInfo};
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
    widgets::{Block, Borders, List, ListItem, Paragraph},
};
use std::{
    error::Error,
    io,
    time::{Duration, Instant},
};

pub struct App {
    pub disassembly: Vec<String>,
    pub symbols: Vec<SymbolInfo>,
    pub current_scroll: usize,
    pub symbol_scroll: usize,
    pub selected_symbol_index: Option<usize>,
    pub show_help: bool,
    pub should_quit: bool,
    pub active_pane: Pane,
}

#[derive(PartialEq)]
pub enum Pane {
    Disassembly,
    Symbols,
}

impl App {
    pub fn new() -> Self {
        Self {
            disassembly: Vec::new(),
            symbols: Vec::new(),
            current_scroll: 0,
            symbol_scroll: 0,
            selected_symbol_index: None,
            show_help: false,
            should_quit: false,
            active_pane: Pane::Disassembly,
        }
    }

    pub fn set_disassembly(&mut self, disassembly: Vec<String>) {
        self.disassembly = disassembly;
    }

    pub fn set_symbols(&mut self, symbols: Vec<SymbolInfo>) {
        self.symbols = symbols;
    }

    pub fn scroll_up(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                if self.current_scroll > 0 {
                    self.current_scroll -= 1;
                }
            }
            Pane::Symbols => {
                if self.symbol_scroll > 0 {
                    self.symbol_scroll -= 1;
                }
            }
        }
    }

    pub fn scroll_down(&mut self) {
        match self.active_pane {
            Pane::Disassembly => {
                if self.current_scroll < self.disassembly.len().saturating_sub(1) {
                    self.current_scroll += 1;
                }
            }
            Pane::Symbols => {
                if self.symbol_scroll < self.symbols.len().saturating_sub(1) {
                    self.symbol_scroll += 1;
                }
            }
        }
    }

    pub fn page_up(&mut self, page_size: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                self.current_scroll = self.current_scroll.saturating_sub(page_size);
            }
            Pane::Symbols => {
                self.symbol_scroll = self.symbol_scroll.saturating_sub(page_size);
            }
        }
    }

    pub fn page_down(&mut self, page_size: usize) {
        match self.active_pane {
            Pane::Disassembly => {
                let max = self.disassembly.len().saturating_sub(1);
                self.current_scroll = (self.current_scroll + page_size).min(max);
            }
            Pane::Symbols => {
                let max = self.symbols.len().saturating_sub(1);
                self.symbol_scroll = (self.symbol_scroll + page_size).min(max);
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
            self.selected_symbol_index = Some(self.symbol_scroll);
            // Here you would typically update the disassembly to show the selected symbol
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
    // Create a layout with two main sections
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(70), Constraint::Percentage(30)].as_ref())
        .split(f.size());

    // Disassembly pane
    let disassembly_items: Vec<ListItem> = app
        .disassembly
        .iter()
        .map(|line| ListItem::new(line.clone()))
        .collect();

    let disassembly_list = List::new(disassembly_items)
        .block(
            Block::default()
                .title("Disassembly")
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
        &mut ratatui::widgets::ListState::default().with_selected(Some(app.current_scroll)),
    );

    // Symbols pane
    let symbol_items: Vec<ListItem> = app
        .symbols
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
                .title("Symbols")
                .borders(Borders::ALL)
                .border_style(if app.active_pane == Pane::Symbols {
                    Style::default().fg(Color::Yellow)
                } else {
                    Style::default()
                }),
        )
        .highlight_style(Style::default().add_modifier(Modifier::BOLD));

    f.render_stateful_widget(
        symbols_list,
        chunks[1],
        &mut ratatui::widgets::ListState::default().with_selected(Some(app.symbol_scroll)),
    );

    // Help overlay
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

