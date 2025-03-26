/// Highlights matched characters in a string based on a pattern.
/// Returns a vector of (start_index, end_index, is_match) tuples.
pub fn highlight_matches(text: &str, pattern: &str) -> Vec<(usize, usize, bool)> {
    if pattern.is_empty() {
        return vec![(0, text.len(), false)];
    }

    let text_lower = text.to_lowercase();
    let pattern_lower = pattern.to_lowercase();

    let mut result = Vec::new();
    let mut text_idx = 0;
    let mut pattern_idx = 0;
    let mut last_non_match_start = 0;

    while text_idx < text.len() && pattern_idx < pattern.len() {
        let text_char = text_lower.chars().nth(text_idx).unwrap();
        let pattern_char = pattern_lower.chars().nth(pattern_idx).unwrap();

        if text_char == pattern_char {
            // Add non-matching segment before this match
            if last_non_match_start < text_idx {
                result.push((last_non_match_start, text_idx, false));
            }

            // Add the matching character
            let char_len = text_char.len_utf8();
            result.push((text_idx, text_idx + char_len, true));

            pattern_idx += 1;
            last_non_match_start = text_idx + char_len;
        }

        text_idx += text_char.len_utf8();
    }

    // Add any remaining text
    if last_non_match_start < text.len() {
        result.push((last_non_match_start, text.len(), false));
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_highlight_matches() {
        let result = highlight_matches("hello", "hl");
        assert_eq!(
            result,
            vec![(0, 1, true), (1, 2, false), (2, 3, true), (3, 5, false)]
        );

        let result = highlight_matches("hello", "");
        assert_eq!(result, vec![(0, 5, false)]);

        let result = highlight_matches("function_name", "fn");
        assert_eq!(
            result,
            vec![(0, 1, true), (1, 2, false), (2, 3, true), (3, 13, false)]
        );
    }
}
