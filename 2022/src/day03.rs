pub fn solve(input: &str) -> (String, String) {
    let lines: Vec<&str> = input.lines().collect();
    let solution_a: usize = lines
        .iter()
        .map(|line| {
            let (first, second) = line.split_at(line.len() / 2);
            [first, second]
        })
        .filter_map(|group| find_common_char(&group))
        .map(priority)
        .sum();
    let solution_b: usize = lines
        .chunks(3)
        .filter_map(find_common_char)
        .map(priority)
        .sum();
    (solution_a.to_string(), solution_b.to_string())
}

fn find_common_char(strings: &[&str]) -> Option<char> {
    let (head, tail) = strings.split_at(1);
    head.first().and_then(|first| {
        first
            .chars()
            .find(|c| tail.iter().all(|string| string.contains(*c)))
    })
}

fn priority(c: char) -> usize {
    if c.is_uppercase() {
        (c as usize) - ('A' as usize) + 27
    } else {
        (c as usize) - ('a' as usize) + 1
    }
}
