use ratatui::style::{Color, Style};
use ratatui::text::Span;

#[derive(Debug, Clone)]
pub struct JumpEdge {
    pub start: u64,
    pub end: u64,
    pub direction: Direction,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Direction {
    Up,
    Down,
}

impl JumpEdge {
    pub fn new(from: u64, to: u64) -> Self {
        let (start, end, direction) = if from < to {
            (from, to, Direction::Down)
        } else {
            (to, from, Direction::Up)
        };
        Self {
            start,
            end,
            direction,
            column: 0,
        }
    }

    pub fn overlaps_with(&self, other: &JumpEdge) -> bool {
        self.start.max(other.start) <= self.end.min(other.end)
    }

    pub fn overlaps(&self, address: u64) -> bool {
        (self.start..=self.end).contains(&address)
    }
}

#[derive(Debug)]
pub struct Jumps {
    jumps: Vec<JumpEdge>,
    max_width: usize,
}

impl Jumps {
    pub fn new(jumps: impl IntoIterator<Item = JumpEdge>) -> Self {
        let mut jumps = jumps.into_iter().collect::<Vec<_>>();
        let max_width = Self::layout(&mut jumps);
        Self { jumps, max_width }
    }

    pub fn jumps_spanning(&self, address: u64) -> impl Iterator<Item = &JumpEdge> {
        self.jumps.iter().filter(move |j| j.overlaps(address))
    }

    pub fn max_width(&self) -> usize {
        self.max_width
    }

    fn layout(jumps: &mut [JumpEdge]) -> usize {
        // Sort jumps by start position
        jumps.sort_by_key(|t| t.start);

        // Track active jumps at each position
        let mut active_columns: Vec<Option<usize>> = Vec::new();

        for jump_idx in 0..jumps.len() {
            // Find the first available column
            let mut column = 0;
            loop {
                // Extend active_columns if needed
                if column >= active_columns.len() {
                    active_columns.push(None);
                    break;
                }

                // Check if this column is available
                let Some(other_idx) = active_columns[column] else {
                    break;
                };

                // Check if the jump in this column overlaps
                let other_jump = &jumps[other_idx];

                if !jumps[jump_idx].overlaps_with(other_jump) {
                    break;
                }

                column += 1;
            }

            let jump = &mut jumps[jump_idx];

            // Assign the column
            jump.column = column;

            // Mark this column as used by this jump
            while column >= active_columns.len() {
                active_columns.push(None);
            }
            active_columns[column] = Some(jump_idx);
        }

        active_columns.len()
    }
}

pub fn render_jumps(address: u64, jumps: &Jumps) -> Vec<Span> {
    let max_width = jumps.max_width();

    let mut columns = vec![None; max_width];
    for jump in jumps.jumps_spanning(address) {
        // insert them into sparse column array (in reverse direction)
        columns[max_width - 1 - jump.column] = Some(jump);
    }

    let mut grid = vec![Span::raw(" "); max_width * 2];

    let mut termination = None;
    for (i, col) in columns.into_iter().enumerate() {
        if let Some(jump) = col {
            let start = address == jump.start;
            let end = address == jump.end;
            let c = if start && end {
                "╶"
            } else if start {
                "┌"
            } else if end {
                "└"
            } else {
                "│"
            };
            if start || end {
                termination = Some(());
            }
            grid[i * 2] = Span::styled(c, Style::default().fg(Color::Green));
            if let Some(termination) = termination {
                grid[i * 2 + 1] = Span::styled("─", Style::default().fg(Color::Green));
            }
        } else if let Some(termination) = termination {
            grid[i * 2 + 0] = Span::styled("─", Style::default().fg(Color::Green));
            grid[i * 2 + 1] = Span::styled("─", Style::default().fg(Color::Green));
        }
    }

    grid
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use ratatui::text::Line;
    use ratatui::widgets::{List, ListItem};

    fn verify_layout(jumps: &Jumps) {
        let jumps = &jumps.jumps;
        for a in 0..jumps.len() {
            for b in 0..jumps.len() {
                if a != b && jumps[a].overlaps_with(&jumps[b]) {
                    assert_ne!(jumps[a].column, jumps[b].column, "overlapping edges");
                }
            }
        }
    }

    fn render_to_string(jumps: &Jumps) -> String {
        verify_layout(jumps);

        let start = jumps.jumps.iter().map(|j| j.start).min().unwrap();
        let end = jumps.jumps.iter().map(|j| j.end).max().unwrap();

        let mut max_width = 0;
        let lines: Vec<_> = (start..=end)
            .map(|addr| {
                let line = Line::from(render_jumps(addr, jumps));
                max_width = line.width().max(max_width);
                ListItem::new(line)
            })
            .collect();

        let mut terminal =
            Terminal::new(TestBackend::new(max_width as u16, (end - start + 1) as u16)).unwrap();
        terminal
            .draw(|frame| frame.render_widget(List::new(lines), frame.area()))
            .unwrap();
        terminal.backend().to_string()
    }

    #[test]
    fn test_single_jump() {
        let jumps = Jumps::new(vec![JumpEdge::new(1, 2)]);

        let rendered = render_to_string(&jumps);
        println!("Single jump:\n{}", rendered);
    }

    #[test]
    fn test_multiple_jumps() {
        let jumps = Jumps::new(vec![
            JumpEdge::new(10, 20),
            JumpEdge::new(15, 25),
            JumpEdge::new(20, 30),
        ]);

        let rendered = render_to_string(&jumps);
        println!("Multiple jumps:\n{}", rendered);
    }

    #[test]
    fn test_backward_jumps() {
        let jumps = Jumps::new(vec![
            JumpEdge::new(2, 1), // Backward jump
            JumpEdge::new(3, 2), // Forward jump
        ]);

        let rendered = render_to_string(&jumps);
        println!("Backward jumps:\n{}", rendered);
    }

    #[test]
    fn test_overlapping_jumps() {
        let jumps = Jumps::new(vec![
            JumpEdge::new(2, 8), // Long jump
            JumpEdge::new(4, 6), // Overlapping jump
        ]);

        let rendered = render_to_string(&jumps);
        println!("Overlapping jumps:\n{}", rendered);
    }

    #[test]
    fn test_self_jumps() {
        let jumps = Jumps::new(vec![
            JumpEdge::new(1, 1), // Self jump
            JumpEdge::new(2, 2), // Another self jump
        ]);

        let rendered = render_to_string(&jumps);
        println!("Self jumps:\n{}", rendered);
    }
}
