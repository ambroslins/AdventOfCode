use nom::{
    bytes::complete::tag,
    character::complete::{digit1, line_ending},
    combinator::map_res,
    multi::separated_list0,
    sequence::separated_pair,
    IResult,
};
use std::{cmp, collections::HashSet, str::FromStr};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
struct Point {
    x: usize,
    y: usize,
}

type Path = Vec<Point>;
fn parse_point(input: &str) -> IResult<&str, Point> {
    let (rest, (x, y)) = separated_pair(
        map_res(digit1, usize::from_str),
        tag(","),
        map_res(digit1, usize::from_str),
    )(input)?;
    Ok((rest, Point { x, y }))
}

fn parse_path(input: &str) -> IResult<&str, Path> {
    separated_list0(tag(" -> "), parse_point)(input)
}

fn build_grid(paths: &Vec<Path>) -> HashSet<Point> {
    let mut set = HashSet::new();
    for path in paths {
        for window in path.windows(2) {
            if let [start, end] = window {
                let x1 = cmp::min(start.x, end.x);
                let x2 = cmp::max(start.x, end.x);
                let y1 = cmp::min(start.y, end.y);
                let y2 = cmp::max(start.y, end.y);
                for x in x1..=x2 {
                    for y in y1..=y2 {
                        set.insert(Point { x, y });
                    }
                }
            }
        }
    }
    set
}

struct State {
    rocks: HashSet<Point>,
    sand: HashSet<Point>,
    floor: usize,
    moving: Point,
}

impl State {
    fn step(&mut self) {
        let x = self.moving.x;
        let y = self.moving.y;
        let next = Point { x, y: y + 1 };
        if next.y >= self.floor {
            self.sand.insert(self.moving);
            self.moving = Point { x: 500, y: 0 };
            return;
        }
        if self.rocks.contains(&next) || self.sand.contains(&next) {
            let left = Point { x: x - 1, y: y + 1 };
            let right = Point { x: x + 1, y: y + 1 };
            if self.rocks.contains(&left) || self.sand.contains(&left) {
                if self.rocks.contains(&right) || self.sand.contains(&right) {
                    self.sand.insert(self.moving);
                    self.moving = Point { x: 500, y: 0 };
                } else {
                    self.moving = right;
                }
            } else {
                self.moving = left;
            }
        } else {
            self.moving = next;
        }
    }
}

fn solve_a(state: &mut State) -> usize {
    while (state.moving.y + 1) < state.floor {
        state.step();
    }
    state.sand.len()
}

fn solve_b(state: &mut State) -> usize {
    while !state.sand.contains(&Point { x: 500, y: 0 }) {
        state.step();
    }
    state.sand.len()
}

pub fn solve(input: &str) -> (String, String) {
    let (_, paths) = separated_list0(line_ending, parse_path)(input).unwrap();
    let rocks = build_grid(&paths);
    let floor = 2 + paths
        .iter()
        .flat_map(|path| path.iter().map(|Point { x: _, y }| y).max())
        .max()
        .unwrap();
    let mut state = State {
        rocks,
        sand: HashSet::new(),
        floor,
        moving: Point { x: 500, y: 0 },
    };
    let solution_a = solve_a(&mut state);
    let solution_b = solve_b(&mut state);
    (solution_a.to_string(), solution_b.to_string())
}
