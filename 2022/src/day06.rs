use std::collections::HashSet;

pub fn solve(input: &str) -> (String, String) {
    (
        find_marker(input, 4).to_string(),
        find_marker(input, 14).to_string(),
    )
}

fn find_marker(input: &str, window_size: usize) -> usize {
    return input
        .as_bytes()
        .windows(window_size)
        .position(|sequence| sequence.iter().collect::<HashSet<_>>().len() == window_size)
        .unwrap()
        + window_size;
}
