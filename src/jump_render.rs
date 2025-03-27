use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use ratatui::style::{Color, Style};
use ratatui::text::Span;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct JumpEdge {
    pub start: u64,
    pub end: u64,
    pub direction: Direction,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    pub fn src(&self) -> u64 {
        match self.direction {
            Direction::Up => self.end,
            Direction::Down => self.start,
        }
    }
    pub fn dst(&self) -> u64 {
        match self.direction {
            Direction::Up => self.start,
            Direction::Down => self.end,
        }
    }

    pub fn color(&self) -> Color {
        let mut hasher = DefaultHasher::new();
        self.dst().hash(&mut hasher);
        let hash = hasher.finish();

        // Use the hash to generate a color
        // We'll use a set of distinct colors that work well together
        let colors = [
            Color::Green,
            Color::Yellow,
            Color::Blue,
            Color::Magenta,
            Color::Cyan,
            Color::Red,
            Color::LightGreen,
            Color::LightYellow,
            Color::LightBlue,
            Color::LightMagenta,
            Color::LightCyan,
            Color::LightRed,
            Color::DarkGray,
        ];

        colors[hash as usize % colors.len()]
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
        #[derive(Debug)]
        struct JumpGroup {
            start: u64,
            end: u64,
            jumps_idx: Vec<usize>,
        }
        impl JumpGroup {
            pub fn overlaps_with(&self, other: &JumpGroup) -> bool {
                self.start.max(other.start) <= self.end.min(other.end)
            }
        }

        // group edges by dst
        let mut groups = {
            let mut bins: HashMap<(u64, Direction), JumpGroup> = HashMap::new();
            for (idx, jump) in jumps.iter().enumerate() {
                let group = bins
                    .entry((jump.dst(), jump.direction))
                    .or_insert(JumpGroup {
                        start: u64::MAX,
                        end: 0,
                        jumps_idx: vec![],
                    });
                group.jumps_idx.push(idx);
                group.start = group.start.min(jump.start);
                group.end = group.start.max(jump.end);
            }
            bins.into_values().collect::<Vec<_>>()
        };

        // Sort jumps by length, then start position
        groups.sort_by_key(|t| (t.end - t.start, t.start));

        // Track active jumps at each position
        let mut columns: Vec<Vec<&JumpGroup>> = Vec::new();

        for group in &groups {
            let mut available = None;
            for (c, column) in columns.iter().enumerate() {
                if column.iter().all(|other| !group.overlaps_with(other)) {
                    available = Some(c);
                    break;
                }
            }
            let column = available.unwrap_or_else(|| {
                columns.push(vec![]);
                columns.len() - 1
            });

            columns[column].push(group);

            // Assign the column
            for jump_idx in &group.jumps_idx {
                jumps[*jump_idx].column = column;
            }
        }

        columns.len()
    }
}

pub fn render_jumps(address: u64, jumps: &Jumps) -> Vec<Span> {
    let max_width = jumps.max_width();

    let mut columns = vec![Vec::new(); max_width];
    for jump in jumps.jumps_spanning(address) {
        // insert them into sparse column array (in reverse direction)
        columns[max_width - 1 - jump.column].push(jump);
    }

    let mut grid = vec![Span::raw(" "); max_width * 2];

    let mut termination = None;
    for (i, col) in columns.into_iter().enumerate() {
        let mut up = false;
        let mut down = false;
        let mut term = false;
        let mut color = Color::default();
        let mut direction = Direction::Down;
        for jump in &col {
            color = jump.color();
            direction = jump.direction;
            let start = address == jump.start;
            let end = address == jump.end;
            if address > jump.start {
                up |= true;
            }
            if address < jump.end {
                down |= true;
            }
            if start || end {
                term |= true;
                termination = Some((direction, color));
            }
        }
        if col.is_empty() {
            if let Some((direction, color)) = termination {
                let c = match direction {
                    Direction::Up => "┉",
                    Direction::Down => "─",
                };
                grid[i * 2 + 0] = Span::styled(c, Style::default().fg(color));
                grid[i * 2 + 1] = Span::styled(c, Style::default().fg(color));
            }
        } else {
            use Direction::*;
            let c = match (direction, up, down, term) {
                (Up, true, true, true) => "┣",
                (Up, true, true, false) => "┋",
                (Up, true, false, true) => "┗",
                (Up, true, false, false) => "╹",
                (Up, false, true, true) => "┏",
                (Up, false, true, false) => "╻",
                (Up, false, false, true) => "╺",
                (Up, false, false, false) => unreachable!(),
                (Down, true, true, true) => "├",
                (Down, true, true, false) => "│",
                (Down, true, false, true) => "└",
                (Down, true, false, false) => "╵",
                (Down, false, true, true) => "┌",
                (Down, false, true, false) => "╷",
                (Down, false, false, true) => "╶",
                (Down, false, false, false) => unreachable!(),
            };
            grid[i * 2] = Span::styled(c, Style::default().fg(color));
            if let Some((direction, color)) = termination {
                let c = match direction {
                    Direction::Up => "┉",
                    Direction::Down => "─",
                };
                grid[i * 2 + 1] = Span::styled(c, Style::default().fg(color));
            }
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
                if a != b {
                    let a = &jumps[a];
                    let b = &jumps[b];
                    if a.overlaps_with(&b) && a.dst() != b.dst() {
                        assert_ne!(a.column, b.column, "overlapping edges");
                    }
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

    #[test]
    fn test_converging() {
        let jumps = Jumps::new(vec![
            JumpEdge::new(2, 7),
            JumpEdge::new(3, 6),
            JumpEdge::new(1, 9),
            JumpEdge::new(0, 9),
        ]);

        let rendered = render_to_string(&jumps);
        println!("Converting\n{}", rendered);
    }

    #[test]
    fn test_collision() {
        let jumps = Jumps::new(vec![JumpEdge::new(2, 7), JumpEdge::new(10, 7)]);

        let rendered = render_to_string(&jumps);
        println!("Collision\n{}", rendered);
    }

    #[test]
    fn test_color_consistency() {
        // Test that the same destination always gets the same color
        let jump1 = JumpEdge::new(1, 10);
        let jump2 = JumpEdge::new(2, 10);
        let jump3 = JumpEdge::new(3, 20);

        assert_eq!(
            jump1.color(),
            jump2.color(),
            "Same destination should have same color"
        );
        assert_ne!(
            jump1.color(),
            jump3.color(),
            "Different destinations should have different colors"
        );
    }
}
