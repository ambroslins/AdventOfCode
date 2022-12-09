use std::{collections::HashSet, str::FromStr};

type Position = (i32, i32);

#[derive(Debug, Clone, Copy)]
enum Direction {
    Left,
    Right,
    Up,
    Down,
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Direction::Left),
            "R" => Ok(Direction::Right),
            "U" => Ok(Direction::Up),
            "D" => Ok(Direction::Down),
            _ => Err("bad direction".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Move {
    direction: Direction,
    steps: usize,
}

impl FromStr for Move {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s.split_once(' ').ok_or_else(|| "bad split".to_string())?;
        let direction = a.parse::<Direction>()?;
        let steps = b.parse::<usize>().map_err(|err| err.to_string())?;
        Ok(Move { direction, steps })
    }
}

fn make_move(position: Position, direction: Direction) -> Position {
    let (x, y) = position;
    match direction {
        Direction::Left => (x - 1, y),
        Direction::Right => (x + 1, y),
        Direction::Up => (x, y + 1),
        Direction::Down => (x, y - 1),
    }
}

#[derive(Debug, Clone)]
struct State<const N: usize> {
    head: Position,
    tail: [Position; N],
}

impl<const N: usize> State<N> {
    fn new() -> Self {
        Self {
            head: (0, 0),
            tail: [(0, 0); N],
        }
    }
    fn step(&mut self, direction: Direction) {
        self.head = make_move(self.head, direction);
        let mut current = self.head;
        for pos in self.tail.iter_mut() {
            *pos = follow(current, *pos);
            current = *pos;
        }
    }
}

fn follow(head: Position, tail: Position) -> Position {
    let (x_head, y_head) = head;
    let (x_tail, y_tail) = tail;
    let dx = x_head - x_tail;
    let dy = y_head - y_tail;
    let distance = dx.abs() + dy.abs();
    if distance > 1 && (dx.abs(), dy.abs()) != (1, 1) {
        (x_tail + dx.signum(), y_tail + dy.signum())
    } else {
        tail
    }
}

fn tail_positions<const N: usize>(directions: &[Direction]) -> usize {
    directions
        .iter()
        .scan(State::<N>::new(), |state, direction| {
            state.step(*direction);
            Some(*state.tail.last().unwrap())
        })
        .collect::<HashSet<Position>>()
        .len()
}

pub fn solve(input: &str) -> (String, String) {
    let directions: Vec<Direction> = input
        .lines()
        .map(|line| line.parse::<Move>().expect("bad parse"))
        .flat_map(|movement| std::iter::repeat(movement.direction).take(movement.steps))
        .collect();
    (
        tail_positions::<1>(&directions).to_string(),
        tail_positions::<9>(&directions).to_string(),
    )
}
