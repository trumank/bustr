use crate::{DisassemblyComment, DisassemblyLine, SymbolKind};
use ratatui::{
    style::{Color, Style},
    text::{Line, Span},
    widgets::ListItem,
};

pub fn format_disassembly_line(line: &DisassemblyLine) -> ListItem {
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

pub fn highlight_operands(operands: &str) -> Vec<Span<'static>> {
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
