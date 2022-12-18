use std::collections::BTreeMap;
use std::fs;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;

type Solution = fn(&str) -> (String, String);

fn main() {
    let solutions: BTreeMap<usize, Solution> = BTreeMap::from([
        (1, day01::solve as Solution),
        (2, day02::solve as Solution),
        (3, day03::solve as Solution),
        (4, day04::solve as Solution),
        (5, day05::solve as Solution),
        (6, day06::solve as Solution),
        (7, day07::solve as Solution),
        (8, day08::solve as Solution),
        (9, day09::solve as Solution),
        (10, day10::solve as Solution),
        (11, day11::solve as Solution),
        (12, day12::solve as Solution),
        (13, day13::solve as Solution),
        (14, day14::solve as Solution),
        (15, day15::solve as Solution),
        (16, day16::solve as Solution),
        (17, day17::solve as Solution),
    ]);
    for arg in std::env::args() {
        if let Ok(day) = arg.parse::<usize>() {
            let solve = solutions.get(&day).unwrap();
            let path = format!("inputs/{:02}.txt", day);
            let input01 = fs::read_to_string(path).expect("input file missing");
            let (a, b) = solve(&input01);
            println!("day {day}");
            println!("{a}");
            println!("{b}");
        }
    }
}
