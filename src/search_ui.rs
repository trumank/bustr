use crate::{
    BinaryData,
    lazy_list::{LazyList, ListItemProducer},
    ui::{jump_to_bottom, jump_to_top},
};
use patternsleuth_image::PatternConfig;
use patternsleuth_scanner::Pattern;
use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, ListItem, ListState, Paragraph, StatefulWidget, Widget},
};

/// Information about a search result
#[derive(Clone, Debug)]
pub struct SearchResult {
    pub address: u64,
    pub data: SearchResultData,
}

/// Data specific to each search type
#[derive(Clone, Debug)]
pub enum SearchResultData {
    XRef { to_address: u64 },
    String { value: String, is_wide: bool },
    BytePattern { pattern: String, bytes: Vec<u8> },
}

/// Navigation entry for search history
#[derive(Clone, Debug)]
pub struct SearchNavigationEntry {
    pub search: Search,
    pub list_state: ListState,
}

/// Add this enum to represent different search types with their parameters
#[derive(Clone, Debug)]
pub enum Search {
    XRef { address: u64 },
    String { string: String },
    BytePattern { pattern: String },
}

/// State for the search widget
pub struct SearchState {
    /// The underlying list state
    list_state: ListState,
    /// Current search query
    pub query: String,
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

    pub fn scroll_up(&mut self, amount: usize) {
        self.list_state.scroll_up_by(amount as u16);
        self.update_nav();
    }

    pub fn scroll_down(&mut self, amount: usize) {
        self.list_state.scroll_down_by(amount as u16);
        self.update_nav();
    }

    pub fn jump_to_top(&mut self) {
        jump_to_top(&mut self.list_state);
        self.update_nav();
    }

    pub fn jump_to_bottom(&mut self) {
        jump_to_bottom(&mut self.list_state, self.results.len());
        self.update_nav();
    }

    fn update_nav(&mut self) {
        if let Some(nav) = self.navigation_stack.get_mut(self.navigation_index) {
            nav.list_state = self.list_state.clone();
        }
        self.dbg_nav("  ");
    }

    /// Sets the search results
    pub fn set_results(&mut self, search: Search, results: Vec<SearchResult>, update_nav: bool) {
        self.results = results;
        self.select(Some(0));

        if update_nav {
            if !self.navigation_stack.is_empty() {
                self.navigation_index += 1;
            }
            let entry = SearchNavigationEntry {
                search,
                list_state: self.list_state.clone(),
            };
            if let Some(existing) = self.navigation_stack.get_mut(self.navigation_index) {
                *existing = entry;
                self.navigation_stack.truncate(self.navigation_index + 1);
            } else {
                self.navigation_stack.push(entry);
            }
        }
        self.dbg_nav("++");
    }
    pub fn navigate_back(&mut self, binary_data: &BinaryData) -> Option<u64> {
        let r = if self.navigation_index > 0 {
            self.navigation_index -= 1;
            let entry = &self.navigation_stack[self.navigation_index];

            // Restore state
            self.query.clear(); // TODO restore query?
            let state = entry.list_state.clone();
            self.search_internal(binary_data, entry.search.clone());
            self.list_state = state;

            self.selected_result().map(|r| r.address)
        } else {
            None
        };
        self.dbg_nav("< ");
        r
    }

    pub fn navigate_forward(&mut self, binary_data: &BinaryData) -> Option<u64> {
        let r = if self.navigation_index < self.navigation_stack.len() - 1 {
            self.navigation_index += 1;
            let entry = &self.navigation_stack[self.navigation_index];

            // Restore state
            self.query.clear(); // TODO restore query?
            let state = entry.list_state.clone();
            self.search_internal(binary_data, entry.search.clone());
            self.list_state = state;

            self.selected_result().map(|r| r.address)
        } else {
            None
        };
        self.dbg_nav(" >");
        r
    }

    fn dbg_nav(&self, _from: &str) {
        //let stack: Vec<_> = self
        //    .navigation_stack
        //    .iter()
        //    .map(|entry| {
        //        format!(
        //            "{}:{}",
        //            entry.list_state.offset(),
        //            entry
        //                .list_state
        //                .selected()
        //                .map(|s| s.to_string())
        //                .unwrap_or("-".to_string())
        //        )
        //    })
        //    .collect();
        //tracing::info!(
        //    "nav {from} index={} len={} stack=[{}]",
        //    self.navigation_index,
        //    self.navigation_stack.len(),
        //    stack.join(" ")
        //);
    }

    pub fn submit_query(&mut self, binary_data: &BinaryData) {
        let query = &self.query;
        let search = if let Some(string) = query.strip_prefix('"').and_then(|q| q.strip_suffix('"'))
        {
            Search::String {
                string: string.into(),
            }
        } else if query
            .split_whitespace()
            .next()
            .is_some_and(|f| f.len() == 2)
        {
            Search::BytePattern {
                pattern: query.to_string(),
            }
        } else {
            Search::XRef {
                address: u64::from_str_radix(&self.query, 16).unwrap(),
            }
        };
        self.search(binary_data, search);
        self.toggle_search_mode();
    }

    /// Add a unified search function that takes a Search enum
    pub fn search(&mut self, binary_data: &BinaryData, search: Search) {
        let results = self.run_search(binary_data, &search);
        self.set_results(search, results, true);
    }

    /// Run search without touching nav
    fn search_internal(&mut self, binary_data: &BinaryData, search: Search) {
        let results = self.run_search(binary_data, &search);
        self.set_results(search, results, false);
    }

    fn run_search(&mut self, binary_data: &BinaryData, search: &Search) -> Vec<SearchResult> {
        match &search {
            Search::XRef { address } => {
                let config = [PatternConfig::xref(
                    (),
                    "".into(),
                    None,
                    patternsleuth_image::scanner::Xref(*address as usize),
                )];

                let res = binary_data.file.scan(&config).unwrap();

                res.results
                    .into_iter()
                    .map(|(_, r)| SearchResult {
                        address: r.address as u64,
                        data: SearchResultData::XRef {
                            to_address: *address,
                        },
                    })
                    .collect()
            }
            Search::String { string } => {
                let utf8 = string.as_bytes().to_vec();
                let utf16 = string
                    .encode_utf16()
                    .flat_map(|c| c.to_le_bytes())
                    .collect::<Vec<_>>();

                #[derive(PartialEq)]
                enum S {
                    A,
                    W,
                }

                let config = [
                    PatternConfig::new(S::A, "".into(), None, Pattern::from_bytes(utf8).unwrap()),
                    PatternConfig::new(S::W, "".into(), None, Pattern::from_bytes(utf16).unwrap()),
                ];

                let res = binary_data.file.scan(&config).unwrap();

                res.results
                    .into_iter()
                    .map(|(s, r)| {
                        let data = binary_data.file.memory.range_from(r.address..).unwrap();
                        let max = 100;
                        let value = match s.sig {
                            S::A => {
                                let len = data[..max].iter().position(|c| *c == 0).unwrap_or(max);
                                String::from_utf8_lossy(&data[..len]).to_string()
                            }
                            S::W => {
                                let chars = data[..max + 2]
                                    .chunks(2)
                                    .map(|c| u16::from_le_bytes(c.try_into().unwrap()))
                                    .take_while(|c| *c != 0)
                                    .collect::<Vec<_>>();
                                String::from_utf16_lossy(&chars).to_string()
                            }
                        };

                        SearchResult {
                            address: r.address as u64,
                            data: SearchResultData::String {
                                value,
                                is_wide: s.sig == S::W,
                            },
                        }
                    })
                    .collect()
            }
            Search::BytePattern { pattern } => {
                let config = [PatternConfig::new(
                    (),
                    "".into(),
                    None,
                    Pattern::new(pattern).unwrap(),
                )];

                let res = binary_data.file.scan(&config).unwrap();

                res.results
                    .into_iter()
                    .map(|(_, r)| SearchResult {
                        address: r.address as u64,
                        data: SearchResultData::BytePattern {
                            pattern: pattern.clone(),
                            bytes: binary_data
                                .file
                                .memory
                                .range(r.address..r.address + pattern.len())
                                .unwrap()
                                .to_vec(),
                        },
                    })
                    .collect()
            }
        }
    }

    /// Gets the selected search result
    pub fn selected_result(&self) -> Option<&SearchResult> {
        self.selected().and_then(|idx| self.results.get(idx))
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
                    SearchResultData::XRef { to_address } => {
                        ListItem::new(Line::from(vec![Span::styled(
                            format!("{:016X} xref {:016X}", result.address, to_address),
                            Style::default().fg(Color::White),
                        )]))
                    }
                    SearchResultData::String { value, is_wide } => {
                        let prefix = if *is_wide { "L" } else { "" };
                        ListItem::new(Line::from(vec![Span::styled(
                            format!("{:016X} : {}\"{}\"", result.address, prefix, value),
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
            format!("Search: {}", state.query)
        } else {
            "Press '/' to search".to_string()
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
