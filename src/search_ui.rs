use crate::{
    BinaryData,
    lazy_list::{LazyList, ListItemProducer},
    ui::{jump_to_bottom, jump_to_top},
};
use patternsleuth_image::PatternConfig;
use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, ListItem, ListState, Paragraph, StatefulWidget, Widget},
};

/// Types of searches
#[derive(Clone, PartialEq, Debug)]
pub enum SearchType {
    XRef,
    String,
    BytePattern,
}

/// Types of cross-references
#[derive(Clone, PartialEq, Debug)]
pub enum XRefKind {
    Call,
    Jump,
    DataRef,
}

/// Information about a search result
#[derive(Clone, Debug)]
pub struct SearchResult {
    pub address: u64,
    pub search_type: SearchType,
    pub data: SearchResultData,
}

/// Data specific to each search type
#[derive(Clone, Debug)]
pub enum SearchResultData {
    XRef {
        to_address: u64,
        kind: XRefKind,
        instruction: String,
    },
    String {
        value: String,
        length: usize,
        is_wide: bool,
    },
    BytePattern {
        pattern: String,
        bytes: Vec<u8>,
    },
}

/// Navigation entry for search history
#[derive(Clone, Debug)]
pub struct SearchNavigationEntry {
    pub search_type: SearchType,
    pub query: String,
    pub list_state: ListState,
}

/// State for the search widget
pub struct SearchState {
    /// The underlying list state
    list_state: ListState,
    /// Current search query
    pub query: String,
    /// Current search type
    pub search_type: SearchType,
    /// Whether we're in search mode
    pub search_mode: bool,
    /// Current search results
    pub results: Vec<SearchResult>,
    /// Navigation stack for searches
    pub navigation_stack: Vec<SearchNavigationEntry>,
    /// Current position in navigation stack
    pub navigation_index: usize,
}

impl SearchState {
    /// Creates a new search state
    pub fn new() -> Self {
        Self {
            list_state: ListState::default(),
            query: String::new(),
            search_type: SearchType::XRef,
            search_mode: false,
            results: Vec::new(),
            navigation_stack: Vec::new(),
            navigation_index: 0,
        }
    }

    /// Toggles search mode
    pub fn toggle_search_mode(&mut self) {
        self.search_mode = !self.search_mode;
        if self.search_mode {
            self.query.clear();
        }
    }

    /// Sets the search type
    pub fn set_search_type(&mut self, search_type: SearchType) {
        self.search_type = search_type;
    }

    /// Adds a character to the query
    pub fn add_to_query(&mut self, c: char) {
        if self.search_mode {
            self.query.push(c);
        }
    }

    /// Removes the last character from the query
    pub fn remove_from_query(&mut self) {
        if self.search_mode {
            self.query.pop();
        }
    }

    /// Gets the selected index
    pub fn selected(&self) -> Option<usize> {
        self.list_state.selected()
    }

    /// Sets the selected index
    pub fn select(&mut self, index: Option<usize>) {
        self.list_state.select(index);
    }

    /// Gets the current offset
    pub fn offset(&self) -> usize {
        self.list_state.offset()
    }

    /// Gets a mutable reference to the offset
    pub fn offset_mut(&mut self) -> &mut usize {
        self.list_state.offset_mut()
    }

    pub fn scroll_up(&mut self, amount: usize) {
        self.list_state.scroll_up_by(amount as u16);
    }

    pub fn scroll_down(&mut self, amount: usize) {
        self.list_state.scroll_down_by(amount as u16);
    }

    pub fn jump_to_top(&mut self) {
        jump_to_top(&mut self.list_state);
    }

    pub fn jump_to_bottom(&mut self) {
        jump_to_bottom(&mut self.list_state, self.results.len());
    }

    /// Sets the search results and updates navigation history
    pub fn set_results(&mut self, results: Vec<SearchResult>) {
        self.results = results;
        self.select(Some(0));

        // Add to navigation stack if this is a new search
        let entry = SearchNavigationEntry {
            search_type: self.search_type.clone(),
            query: self.query.clone(),
            list_state: ListState::default(),
        };

        // If we're navigating through history, update the current entry
        if self.navigation_index < self.navigation_stack.len() {
            // Only push to navigation stack if this is a new navigation (not from history)
            self.navigation_index += 1;

            if let Some(existing) = self.navigation_stack.get_mut(self.navigation_index) {
                *existing = entry;
                // Truncate the stack to remove any forward history
                self.navigation_stack.truncate(self.navigation_index + 1);
            } else {
                self.navigation_stack.push(entry);
            }
        } else {
            // This is a new search, add it to the stack
            self.navigation_stack.push(entry);
            self.navigation_index = self.navigation_stack.len() - 1;
        }
    }

    /// Navigates back in the search history
    pub fn navigate_back(&mut self) {
        if self.navigation_index > 0 {
            // Save current state
            if let Some(current) = self.navigation_stack.get_mut(self.navigation_index) {
                current.list_state = self.list_state.clone();
            }

            // Move back
            self.navigation_index -= 1;
            let entry = &self.navigation_stack[self.navigation_index];

            // Restore state
            self.search_type = entry.search_type.clone();
            self.query = entry.query.clone();
            self.list_state = entry.list_state.clone();

            // Need to reload search results for this query
            // This would typically be done by the App
        }
    }

    /// Navigates forward in the search history
    pub fn navigate_forward(&mut self) {
        if self.navigation_index < self.navigation_stack.len() - 1 {
            // Save current state
            if let Some(current) = self.navigation_stack.get_mut(self.navigation_index) {
                current.list_state = self.list_state.clone();
            }

            // Move forward
            self.navigation_index += 1;
            let entry = &self.navigation_stack[self.navigation_index];

            // Restore state
            self.search_type = entry.search_type.clone();
            self.query = entry.query.clone();
            self.list_state = entry.list_state.clone();

            // Need to reload search results for this query
            // This would typically be done by the App
        }
    }

    /// Gets the selected search result
    pub fn selected_result(&self) -> Option<&SearchResult> {
        self.selected().and_then(|idx| self.results.get(idx))
    }

    /// Finds XRefs for an address in the binary
    pub fn find_xrefs(&mut self, binary_data: &BinaryData, address: u64) {
        let config = [PatternConfig::xref(
            (),
            "".into(),
            None,
            patternsleuth_image::scanner::Xref(address as usize),
        )];

        let res = binary_data.file.scan(&config).unwrap();

        self.set_results(
            res.results
                .into_iter()
                .map(|(_, r)| SearchResult {
                    address: r.address as u64,
                    search_type: SearchType::XRef,
                    data: SearchResultData::XRef {
                        to_address: address,
                        kind: XRefKind::Call,
                        instruction: format!("call 0x{:X}", address),
                    },
                })
                .collect(),
        );
    }

    /// Finds strings in the binary
    pub fn find_strings(&mut self, binary_data: &BinaryData, min_length: usize) {
        // Stub implementation - would scan for strings in the binary
        let mut results = Vec::new();

        // Just add a placeholder result for now
        results.push(SearchResult {
            address: 0x1000,
            search_type: SearchType::String,
            data: SearchResultData::String {
                value: "Example string".to_string(),
                length: 14,
                is_wide: false,
            },
        });

        self.set_results(results);
    }

    /// Finds byte patterns in the binary
    pub fn find_byte_pattern(&mut self, binary_data: &BinaryData, pattern: &str) {
        // Stub implementation - would scan for the byte pattern in the binary
        let mut results = Vec::new();

        // Just add a placeholder result for now
        results.push(SearchResult {
            address: 0x2000,
            search_type: SearchType::BytePattern,
            data: SearchResultData::BytePattern {
                pattern: pattern.to_string(),
                bytes: vec![0x90, 0x90, 0x90], // Example: NOP NOP NOP
            },
        });

        self.set_results(results);
    }
}

pub struct SearchWidget {
    active: bool,
}

impl SearchWidget {
    pub fn new(active: bool) -> Self {
        Self { active }
    }
}

impl StatefulWidget for SearchWidget {
    type State = SearchState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        struct SearchListProd<'a>(&'a [SearchResult]);
        impl<'a> ListItemProducer<'a> for SearchListProd<'a> {
            fn total(&self) -> usize {
                self.0.len()
            }
            fn items(
                &self,
                range: std::ops::Range<usize>,
            ) -> impl ExactSizeIterator<Item = ListItem<'a>> {
                self.0[range].iter().map(|result| match &result.data {
                    SearchResultData::XRef {
                        to_address,
                        kind,
                        instruction,
                    } => {
                        let kind_str = match kind {
                            XRefKind::Call => "call",
                            XRefKind::Jump => "jmp",
                            XRefKind::DataRef => "ref",
                        };
                        ListItem::new(Line::from(vec![Span::styled(
                            format!("{:016X} {} {:016X}", result.address, kind_str, to_address),
                            Style::default().fg(Color::White),
                        )]))
                    }
                    SearchResultData::String {
                        value,
                        length,
                        is_wide,
                    } => {
                        let prefix = if *is_wide { "L" } else { "" };
                        ListItem::new(Line::from(vec![Span::styled(
                            format!(
                                "{:016X} string ({} chars): {}\"{}\"",
                                result.address, length, prefix, value
                            ),
                            Style::default().fg(Color::Green),
                        )]))
                    }
                    SearchResultData::BytePattern { pattern, bytes } => {
                        let bytes_str = bytes
                            .iter()
                            .map(|b| format!("{:02X}", b))
                            .collect::<Vec<_>>()
                            .join(" ");
                        ListItem::new(Line::from(vec![Span::styled(
                            format!(
                                "{:016X} pattern: {} [{}]",
                                result.address, pattern, bytes_str
                            ),
                            Style::default().fg(Color::Cyan),
                        )]))
                    }
                })
            }
        }

        // Calculate the inner area of the XRefs block
        let block = Block::default()
            .title(format!("Results ({} found)", state.results.len()))
            .borders(Borders::ALL)
            .border_style(if self.active {
                Style::default().fg(Color::Yellow)
            } else {
                Style::default()
            });

        Widget::render(&block, area, buf);

        // Split the inner area for query bar and XRefs list
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Query bar takes just one line
                Constraint::Min(1),    // Results list takes the rest
            ])
            .split(block.inner(area));

        // Always render the query bar, but style it differently when not in search mode
        let query_text = if state.search_mode {
            format!("Address: {}", state.query)
        } else {
            "Press 'x' to enter address".to_string()
        };

        let query_style = if state.search_mode {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let query_bar = Paragraph::new(query_text).style(query_style);
        let list = LazyList::new(SearchListProd(&state.results));

        Widget::render(query_bar, chunks[0], buf);
        StatefulWidget::render(list, chunks[1], buf, &mut state.list_state);
    }
}
