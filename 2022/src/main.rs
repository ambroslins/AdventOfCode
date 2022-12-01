use std::fs;
mod day01;
fn main() {
    let input01 = fs::read_to_string("inputs/01.txt").expect("no input file");
    let (a, b) = day01::solve(&input01);
    println!("01: {}, {}", a, b);
    println!("Hello, world!");
}
