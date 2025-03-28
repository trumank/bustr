use crate::InstructionKind;
use crate::jump_render::{JumpEdge, Jumps};
use crate::{BinaryData, DisassemblyComment, DisassemblyLine, SymbolKind, ui::NavigationEntry};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, ListItem, ListState, StatefulWidget},
};

/// State for the disassembly widget that wraps the existing ListState
pub struct DisassemblyState {
    /// The underlying list state
    list_state: ListState,
    /// Current address being viewed
    pub current_address: u64,
    /// Disassembly lines
    pub disassembly: Vec<DisassemblyLine>,
    /// Navigation stack
    pub navigation_stack: Vec<NavigationEntry>,
    /// Current position in navigation stack
    pub navigation_index: usize,
    /// Last refresh address
    last_refresh_address: u64,
}

impl DisassemblyState {
    /// Creates a new disassembly state
    pub fn new() -> Self {
        let mut list_state = ListState::default();
        list_state.select(Some(0));

        Self {
            list_state,
            current_address: 0,
            disassembly: Vec::new(),
            navigation_stack: Vec::new(),
            navigation_index: 0,
            last_refresh_address: 0,
        }
    }

    /// Sets the current address and updates the disassembly if needed
    pub fn set_current_address(&mut self, address: u64) {
        tracing::info!("set address {address:x}");
        self.current_address = address;

        // Only push to navigation stack if this is a new navigation (not from history)
        if !self.navigation_stack.is_empty() {
            self.navigation_index += 1;
        }

        let entry = NavigationEntry {
            address,
            scroll_offset: self.offset(),
        };

        if let Some(existing) = self.navigation_stack.get_mut(self.navigation_index) {
            *existing = entry;
        } else {
            self.navigation_stack.push(entry);
        }

        self.set_address(address);
    }

    /// Updates the disassembly if needed
    fn update_disassembly(&mut self, binary_data: &BinaryData, height: usize) {
        // Only update if we have binary data and the address has changed
        if self.current_address != self.last_refresh_address {
            const FROM_TOP: usize = 15;

            let mut first_line = None;
            let disassembly =
                crate::disassemble_range(binary_data, self.current_address - 50, &mut |dis| {
                    if first_line.is_none() {
                        first_line = dis
                            .iter()
                            .position(|l| l.address().is_some_and(|a| a > self.current_address))
                            .map(|l| l - 1)
                    }
                    if let Some(first_line) = first_line {
                        dis.len() + FROM_TOP < height + first_line
                    } else {
                        true
                    }
                })
                .unwrap();

            let selected = first_line.unwrap_or(0);
            self.disassembly = disassembly;
            self.select(Some(selected));
            *self.offset_mut() = selected.saturating_sub(FROM_TOP);

            self.last_refresh_address = self.current_address;
        }
    }

    /// Sets the current address
    pub fn set_address(&mut self, address: u64) {
        self.current_address = address;
    }

    /// Gets the current address
    pub fn current_address(&self) -> u64 {
        self.current_address
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

    pub fn scroll_up(&mut self, binary_data: &BinaryData, amount: usize) {
        if let Some(mut selected) = self.selected() {
            // ensure margin of 10 off screen
            if selected > 10 + amount {
                selected -= amount;
            } else if !self.disassembly.is_empty() {
                let new_lines = self.prepend_disassembly(binary_data, amount + 3);

                selected -= amount - new_lines;
                *self.offset_mut() += new_lines;
            }
            self.select(Some(selected));

            // Update the current navigation entry's scroll offset
            let offset = self.offset();
            if let Some(entry) = self.navigation_stack.get_mut(self.navigation_index) {
                entry.scroll_offset = offset;
                entry.address = self.disassembly[selected].address_block();
            }
        }
    }

    pub fn scroll_down(&mut self, binary_data: &BinaryData, amount: usize) {
        if let Some(mut selected) = self.selected() {
            let max = self.disassembly.len().saturating_sub(1);
            if selected + amount < max {
                selected += amount;
            } else {
                if !self.disassembly.is_empty() {
                    self.append_disassembly(binary_data, amount + 3);
                }
                let max = self.disassembly.len().saturating_sub(1);
                selected = (selected + amount).min(max);
            }
            self.select(Some(selected));

            // Update the current navigation entry's scroll offset
            let offset = self.offset();
            if let Some(entry) = self.navigation_stack.get_mut(self.navigation_index) {
                entry.scroll_offset = offset;
                entry.address = self.disassembly[selected].address_block();
            }
        }
    }

    /// load at least new_lines and prepend them to the current disassembly
    /// return how many lines were actually added
    /// also replaces first N lines because they could be improperly disassembled
    pub fn prepend_disassembly(&mut self, binary_data: &BinaryData, new_lines: usize) -> usize {
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
                if let Some(until) = block_iter
                    .find(|block| block.address_range().end >= first_address + REPLACE_BYTES as u64)
                {
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

        added_lines
    }

    pub fn append_disassembly(&mut self, binary_data: &BinaryData, new_lines: usize) {
        if let Some(last) = self.disassembly.iter().rev().find_map(|l| l.address_end()) {
            let new_dis =
                crate::disassemble_range(binary_data, last, &mut |dis| dis.len() < new_lines)
                    .unwrap();
            self.disassembly.extend(new_dis);
        }
    }

    pub fn navigate_back(&mut self, _binary_data: &BinaryData) {
        if self.navigation_index > 0 {
            self.navigation_index -= 1;
            let entry = &self.navigation_stack[self.navigation_index];
            self.current_address = entry.address;
            *self.offset_mut() = entry.scroll_offset;
        }
    }

    pub fn navigate_forward(&mut self, _binary_data: &BinaryData) {
        if self.navigation_index < self.navigation_stack.len() - 1 {
            self.navigation_index += 1;
            let entry = &self.navigation_stack[self.navigation_index];
            self.current_address = entry.address;
            *self.offset_mut() = entry.scroll_offset;
        }
    }

    pub fn follow_address_reference(&mut self) {
        if let Some(DisassemblyLine::Instruction {
            referenced_address: Some(addr),
            ..
        }) = self.selected().and_then(|s| self.disassembly.get(s))
        {
            self.set_current_address(*addr);
        }
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

/// Disassembly widget that wraps the existing list widget
pub struct DisassemblyWidget<'a, 'data> {
    /// Block to wrap the widget in
    block: Option<Block<'a>>,
    /// Highlight style for selected items
    highlight_style: Style,

    binary_data: Option<&'a BinaryData<'data>>,
}

impl<'a, 'data> DisassemblyWidget<'a, 'data> {
    /// Creates a new disassembly widget
    pub fn new(binary_data: Option<&'a BinaryData<'data>>) -> Self {
        Self {
            block: None,
            highlight_style: Style::default().add_modifier(Modifier::BOLD | Modifier::UNDERLINED),
            binary_data,
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

impl StatefulWidget for DisassemblyWidget<'_, '_> {
    type State = DisassemblyState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        // Update the disassembly if needed
        if let Some(binary_data) = self.binary_data {
            let height = area.height as usize;
            state.update_disassembly(binary_data, height);

            // fill up the screen in case it was expanded
            let visible_lines = state
                .disassembly
                .len()
                .saturating_sub(state.list_state.offset());
            if height > visible_lines {
                state.append_disassembly(binary_data, height - visible_lines);
            }
        }

        let jumps = state.disassembly.iter().flat_map(|line| match line {
            DisassemblyLine::Instruction {
                address,
                referenced_address: Some(referenced),
                kind,
                ..
            } if *kind == InstructionKind::Jump => Some(JumpEdge::new(*address, *referenced)),
            _ => None,
        });

        let jumps = Jumps::new(jumps);

        // Convert disassembly lines to list items
        let items: Vec<ListItem> = state
            .disassembly
            .iter()
            .map(|line| format_disassembly_line(line, &jumps))
            .collect();

        // Create the list widget
        let list = ratatui::widgets::List::new(items)
            .block(self.block.unwrap_or_default())
            .highlight_style(self.highlight_style);

        // Render the list widget with the underlying list state
        StatefulWidget::render(list, area, buf, &mut state.list_state);
    }
}

/// Formats a disassembly line into a styled ListItem for display
fn format_disassembly_line<'a>(line: &'a DisassemblyLine, jumps: &'a Jumps) -> ListItem<'a> {
    match line {
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
            ..
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

            // Add jump rendering
            spans.extend(crate::jump_render::render_jumps(*address, jumps));

            // Add instruction
            spans.extend(instruction.clone());

            // Add comments
            if !comments.is_empty() {
                spans.push(Span::styled(" ; ", Style::default().fg(Color::DarkGray)));

                for (i, comment) in comments.iter().enumerate() {
                    if i > 0 {
                        spans.push(Span::raw(" "));
                    }

                    match comment {
                        DisassemblyComment::BranchTarget(sym) => {
                            spans.push(Span::styled("-> ", Style::default().fg(Color::DarkGray)));
                            spans.push(Span::styled(
                                sym.display_name(),
                                Style::default().fg(Color::Yellow),
                            ));
                        }
                        DisassemblyComment::MemoryReference(sym) => {
                            spans.push(Span::styled("ref ", Style::default().fg(Color::DarkGray)));
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
    }
}

pub fn format_data_spans(data: &[u8]) -> Span {
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
