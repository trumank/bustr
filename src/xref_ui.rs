use crate::{
    BinaryData,
    lazy_list::{LazyList, ListItemProducer},
    ui::{jump_to_bottom, jump_to_top},
};
use patternsleuth_image::PatternConfig;
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, ListItem, ListState, StatefulWidget},
};

/// Types of cross-references
#[derive(Clone, PartialEq, Debug)]
pub enum XRefKind {
    Call,
    Jump,
    DataRef,
}

/// Information about a cross-reference
#[derive(Clone, Debug)]
pub struct XRefInfo {
    pub from_address: u64,
    pub to_address: u64,
    pub kind: XRefKind,
    pub instruction: String,
}

/// Navigation entry for XRef history
#[derive(Clone, Debug)]
pub struct XRefNavigationEntry {
    pub address: u64,
    pub query: String,
    pub list_state: ListState,
}

/// State for the XRef widget
pub struct XRefState {
    /// The underlying list state
    list_state: ListState,
    /// Current search query
    pub query: String,
    /// Whether we're in XRef search mode
    pub search_mode: bool,
    /// Current XRefs
    pub xrefs: Vec<XRefInfo>,
    /// Navigation stack for XRef searches
    pub navigation_stack: Vec<XRefNavigationEntry>,
    /// Current position in navigation stack
    pub navigation_index: usize,
}

impl XRefState {
    /// Creates a new XRef state
    pub fn new() -> Self {
        Self {
            list_state: ListState::default(),
            query: String::new(),
            search_mode: false,
            xrefs: Vec::new(),
            navigation_stack: Vec::new(),
            navigation_index: 0,
        }
    }

    /// Toggles XRef search mode
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
        jump_to_bottom(&mut self.list_state, self.xrefs.len());
    }

    /// Sets the XRefs and updates navigation history
    pub fn set_xrefs(&mut self, xrefs: Vec<XRefInfo>, address: u64) {
        self.xrefs = xrefs;
        self.select(Some(0));

        // Add to navigation stack if this is a new search
        let entry = XRefNavigationEntry {
            address,
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

    /// Navigates back in the XRef history
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
            self.query = entry.query.clone();
            self.list_state = entry.list_state.clone();

            // Need to reload XRefs for this address
            // This would typically be done by the App
        }
    }

    /// Navigates forward in the XRef history
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
            self.query = entry.query.clone();
            self.list_state = entry.list_state.clone();

            // Need to reload XRefs for this address
            // This would typically be done by the App
        }
    }

    /// Gets the current address from navigation history
    pub fn current_address(&self) -> Option<u64> {
        self.navigation_stack
            .get(self.navigation_index)
            .map(|e| e.address)
    }

    /// Gets the selected XRef
    pub fn selected_xref(&self) -> Option<&XRefInfo> {
        self.selected().and_then(|idx| self.xrefs.get(idx))
    }

    pub fn find_xrefs(&mut self, binary_data: &BinaryData, address: u64) {
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
            0,
        );
    }
}

/// XRef widget for displaying cross-references
pub struct XRefWidget<'a> {
    /// Block to wrap the widget in
    block: Option<Block<'a>>,
    /// Highlight style for selected items
    highlight_style: Style,
}

impl<'a> XRefWidget<'a> {
    /// Creates a new XRef widget
    pub fn new() -> Self {
        Self {
            block: None,
            highlight_style: Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED),
        }
    }

    /// Sets the block for the widget
    pub fn block(mut self, block: Block<'a>) -> Self {
        self.block = Some(block);
        self
    }

    /// Sets the highlight style
    pub fn highlight_style(mut self, style: Style) -> Self {
        self.highlight_style = style;
        self
    }
}

impl StatefulWidget for XRefWidget<'_> {
    type State = XRefState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
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

        let list = LazyList::new(XrefListProd(&state.xrefs));

        StatefulWidget::render(list, area, buf, &mut state.list_state);
    }
}
