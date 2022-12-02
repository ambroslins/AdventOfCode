use std::collections::BTreeMap;
use std::fs;
mod day01;
mod day02;

type Solution = fn(&str) -> (String, String);
fn main() {
    let solutions: BTreeMap<usize, Solution> =
        BTreeMap::from([(1, day01::solve as Solution), (2, day02::solve as Solution)]);

    for (day, solve) in &solutions {
        let path = format!("inputs/{:02}.txt", day);
        let input01 = fs::read_to_string(path).expect("input file missing");
        let (a, b) = solve(&input01);
        println!("day {} a: {}, {}", day, a, b);
    }
}
