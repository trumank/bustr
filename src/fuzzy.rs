//! Fuzzy matching module for symbol search

/// Performs a fuzzy match between a text and a pattern.
/// Returns a score if there's a match, or None if no match.
/// Higher scores indicate better matches.
pub fn fuzzy_match(text: &str, pattern: &str) -> Option<u32> {
    if pattern.is_empty() {
        return Some(0);
    }

    let text = text.to_lowercase();
    let pattern = pattern.to_lowercase();

    let mut score = 0;
    let mut text_idx = 0;
    let mut pattern_idx = 0;
    let mut last_match_idx = 0;
    let mut consecutive_matches = 0;

    // Try to match all characters in the pattern
    while pattern_idx < pattern.len() && text_idx < text.len() {
        let pattern_char = pattern.chars().nth(pattern_idx).unwrap();
        let text_char = text.chars().nth(text_idx).unwrap();

        if pattern_char == text_char {
            // Match found
            pattern_idx += 1;

            // Scoring: prefer matches at word boundaries
            if text_idx == 0
                || text
                    .chars()
                    .nth(text_idx - 1)
                    .map_or(false, |c| !c.is_alphanumeric())
            {
                score += 10; // Word boundary bonus
            }

            // Scoring: prefer consecutive matches
            if text_idx == last_match_idx + 1 {
                consecutive_matches += 1;
                score += consecutive_matches * 5; // Consecutive match bonus
            } else {
                consecutive_matches = 0;
            }

            // Scoring: prefer matches near the beginning
            score += 100 - (text_idx as u32).min(100);

            last_match_idx = text_idx;
        }

        text_idx += 1;
    }

    // If we matched all pattern characters, return the score
    if pattern_idx == pattern.len() {
        Some(score)
    } else {
        None
    }
}

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
    fn test_fuzzy_match() {
        // Exact matches
        assert!(fuzzy_match("hello", "hello").is_some());

        // Partial matches
        assert!(fuzzy_match("hello world", "hlo").is_some());
        assert!(fuzzy_match("function_name", "func").is_some());

        // No matches
        assert!(fuzzy_match("hello", "xyz").is_none());

        // Empty pattern always matches
        assert_eq!(fuzzy_match("hello", ""), Some(0));

        // Scoring tests
        assert!(fuzzy_match("hello", "h").unwrap() > fuzzy_match("hello", "o").unwrap()); // Prefer early matches
        assert!(
            fuzzy_match("hello_world", "hw").unwrap() > fuzzy_match("helloworld", "hw").unwrap()
        ); // Prefer word boundaries
        assert!(fuzzy_match("hello", "hel").unwrap() > fuzzy_match("hello", "hlo").unwrap()); // Prefer consecutive matches
    }

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
            vec![(0, 1, true), (1, 8, false), (8, 9, true), (9, 13, false)]
        );
    }
}
