use std::collections::VecDeque;

fn mix(sequence: &Vec<i64>, times: usize) -> Vec<i64> {
    let n = sequence.len() as i64;
    let mut decrypted: VecDeque<(usize, i64)> = sequence.iter().copied().enumerate().collect();
    for _ in 0..times {
        for idx in 0..sequence.len() {
            let i = decrypted.iter().position(|(i, _)| *i == idx).unwrap();
            decrypted.rotate_left(i);
            let (_, y) = decrypted.pop_front().unwrap();
            let j = y.rem_euclid(n - 1) as usize;
            decrypted.insert(j, (idx, y));
        }
    }
    decrypted.iter().map(|(_, x)| *x).collect()
}

fn grove_coordinates(decrypted: &Vec<i64>) -> i64 {
    let i = decrypted.iter().position(|x| *x == 0).unwrap();
    let n = decrypted.len();
    [1000, 2000, 3000]
        .iter()
        .map(|j| decrypted.get((i + j).rem_euclid(n)).unwrap())
        .sum()
}

fn solve_a(sequence: &Vec<i64>) -> i64 {
    let decrypted = mix(sequence, 1);
    grove_coordinates(&decrypted)
}

fn solve_b(sequence: &[i64]) -> i64 {
    let key = 811589153;
    let decrypted = mix(&sequence.iter().map(|x| x * key).collect(), 10);
    grove_coordinates(&decrypted)
}

pub fn solve(input: &str) -> (String, String) {
    let file: Vec<i64> = input.lines().map(|line| line.parse().unwrap()).collect();

    (solve_a(&file).to_string(), solve_b(&file).to_string())
}
