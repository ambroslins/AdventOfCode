use partial_sort::PartialSort;

pub fn solve(input: &str) -> (usize, usize) {
    let lines: Vec<&str> = input.lines().collect();
    let mut sums: Vec<usize> = lines
        .split(|line: &&str| line.is_empty())
        .map(|numbers| {
            numbers
                .iter()
                .flat_map(|number| number.parse::<usize>())
                .sum()
        })
        .collect();
    sums.partial_sort(3, |a, b| b.cmp(a));
    return (*sums.first().unwrap(), sums.iter().take(3).sum());
}
