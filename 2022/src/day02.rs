use std::{cmp::Ordering, str::FromStr};

#[derive(PartialEq, Eq, Copy, Clone)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl Ord for Shape {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (a, b) if a == b => Ordering::Equal,
            (Shape::Rock, Shape::Scissors)
            | (Shape::Scissors, Shape::Paper)
            | (Shape::Paper, Shape::Rock) => Ordering::Greater,
            _ => Ordering::Less,
        }
    }
}
impl PartialOrd for Shape {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl FromStr for Shape {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" | "X" => Ok(Shape::Rock),
            "B" | "Y" => Ok(Shape::Paper),
            "C" | "Z" => Ok(Shape::Scissors),
            _ => Err(()),
        }
    }
}

impl Shape {
    fn score(&self) -> usize {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }
}

fn outcome_score(a: Shape, b: Shape) -> usize {
    match a.cmp(&b) {
        Ordering::Less => 0,
        Ordering::Equal => 3,
        Ordering::Greater => 6,
    }
}

fn round_score(a: Shape, b: Shape) -> usize {
    a.score() + outcome_score(a, b)
}

type Outcome = Ordering;

pub fn solve(input: &str) -> (String, String) {
    return (solve_a(input), solve_b(input));
}

fn solve_a(input: &str) -> String {
    return input
        .lines()
        .into_iter()
        .flat_map(|line: &str| {
            line.split_once(" ")
                .and_then(|(a, b)| match (a.parse::<Shape>(), b.parse::<Shape>()) {
                    (Ok(x), Ok(y)) => Some((x, y)),
                    _ => None,
                })
        })
        .map(|(opponent, my)| round_score(my, opponent))
        .sum::<usize>()
        .to_string();
}

fn parse_outcome(s: &str) -> Option<Outcome> {
    match s {
        "X" => Some(Ordering::Less),
        "Y" => Some(Ordering::Equal),
        "Z" => Some(Ordering::Greater),
        _ => None,
    }
}

fn shape_for_outcome(shape: Shape, outcome: Outcome) -> Shape {
    match outcome {
        Ordering::Less => match shape {
            Shape::Rock => Shape::Scissors,
            Shape::Paper => Shape::Rock,
            Shape::Scissors => Shape::Paper,
        },
        Ordering::Equal => shape,
        Ordering::Greater => match shape {
            Shape::Rock => Shape::Paper,
            Shape::Paper => Shape::Scissors,
            Shape::Scissors => Shape::Rock,
        },
    }
}

fn solve_b(input: &str) -> String {
    return input
        .lines()
        .into_iter()
        .flat_map(|line: &str| {
            line.split_once(" ")
                .and_then(|(a, b)| match (a.parse::<Shape>(), parse_outcome(b)) {
                    (Ok(x), Some(y)) => Some((x, y)),
                    _ => None,
                })
        })
        .map(|(opponent, outcome)| round_score(shape_for_outcome(opponent, outcome), opponent))
        .sum::<usize>()
        .to_string();
}
