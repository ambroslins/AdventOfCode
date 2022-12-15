use std::{cmp, slice::Iter};

use nom::{
    bytes::complete::tag,
    character::complete::{self, line_ending},
    combinator::map,
    multi::separated_list1,
    sequence::{pair, preceded, separated_pair},
    IResult,
};

#[derive(Debug, Copy, Clone)]
struct Position {
    x: i32,
    y: i32,
}

fn parse_position(input: &str) -> IResult<&str, Position> {
    map(
        separated_pair(
            preceded(tag("x="), complete::i32),
            tag(", "),
            preceded(tag("y="), complete::i32),
        ),
        |(x, y)| Position { x, y },
    )(input)
}

#[derive(Debug, Clone, Copy)]
struct Reading {
    sensor: Position,
    beacon: Position,
}

fn parse_reading(input: &str) -> IResult<&str, Reading> {
    map(
        pair(
            preceded(tag("Sensor at "), parse_position),
            preceded(tag(": closest beacon is at "), parse_position),
        ),
        |(sensor, beacon)| Reading { sensor, beacon },
    )(input)
}

#[derive(Debug, Clone, Copy)]
struct Interval {
    start: i32,
    end: i32,
}

impl Interval {
    fn len(&self) -> usize {
        assert!(self.end >= self.start);
        (self.end - self.start) as usize
    }
}

#[derive(Debug, Clone)]
struct Intervals(Vec<Interval>);

impl Intervals {
    fn new() -> Self {
        Intervals(Vec::new())
    }

    fn insert(&mut self, interval: Interval) {
        if interval.end < interval.start {
            return;
        }
        let i = self.0.partition_point(|int| int.end < interval.start);
        let j = self.0.partition_point(|int| int.start <= interval.end);
        let new = self.0.drain(i..j).fold(interval, |acc, int| {
            let start = cmp::min(acc.start, int.start);
            let end = cmp::max(acc.end, int.end);
            Interval { start, end }
        });
        self.0.insert(i, new);
    }

    fn iter(&self) -> Iter<'_, Interval> {
        self.0.iter()
    }
}

fn scan_line(readings: &Vec<Reading>, y: i32) -> Intervals {
    let mut intervals = Intervals::new();
    for reading in readings {
        let sensor = &reading.sensor;
        let beacon = &reading.beacon;
        let dx = beacon.x - sensor.x;
        let dy = beacon.y - sensor.y;
        let distance = dx.abs() + dy.abs();
        let x_offset = distance - (y - sensor.y).abs();
        let interval = Interval {
            start: sensor.x - x_offset,
            end: sensor.x + x_offset,
        };
        intervals.insert(interval);
    }
    intervals
}

fn solve_a(readings: &Vec<Reading>) -> usize {
    scan_line(readings, 2000000)
        .iter()
        .map(|interval| interval.len())
        .sum()
}

fn find_distress_signal(intervals: &Intervals) -> Option<usize> {
    intervals
        .0
        .windows(2)
        .flat_map(|is| match is {
            [left, right] if left.end >= 0 && right.start <= 4000000 => Some(left.end as usize + 1),
            _ => None,
        })
        .next()
}

fn solve_b(readings: &Vec<Reading>) -> usize {
    (0..=4000000)
        .flat_map(|y| {
            let intervals = scan_line(readings, y);
            find_distress_signal(&intervals).map(|x| x * 4000000 + y as usize)
        })
        .next()
        .unwrap()
}

pub fn solve(input: &str) -> (String, String) {
    let (_, readings) = separated_list1(line_ending, parse_reading)(input).unwrap();
    (
        solve_a(&readings).to_string(),
        solve_b(&readings).to_string(),
    )
}
