use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
};

use ndarray::Array2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Position {
    x: usize,
    y: usize,
}

#[derive(Debug)]
struct HeightMap {
    grid: Array2<u32>,
    start: Position,
    end: Position,
}

#[derive(Debug, PartialEq, Eq)]
struct Node {
    steps: usize,
    height: u32,
    position: Position,
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match other.steps.cmp(&self.steps) {
            Ordering::Equal => self.height.cmp(&other.height),
            ord => ord,
        }
    }
}

fn parse_input(input: &str) -> HeightMap {
    let mut start = Position { x: 0, y: 0 };
    let mut end = Position { x: 0, y: 0 };
    let height = input.lines().count();
    let width = input.lines().next().unwrap().len();
    let grid: Array2<u32> = Array2::from_shape_vec(
        (height, width),
        input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.char_indices()
                    .map(|(x, char)| match char {
                        'S' => {
                            start = Position { x, y };
                            0
                        }
                        'E' => {
                            end = Position { x, y };
                            25
                        }
                        _ => char as u32 - ('a' as u32),
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<u32>>(),
    )
    .unwrap();
    HeightMap { grid, start, end }
}

fn shortest_path(grid: &Array2<u32>, start: Position, done: impl Fn(&Node) -> bool) -> usize {
    let offsets: [(i32, i32); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];
    let mut nodes = BinaryHeap::from([Node {
        steps: 0,
        height: 0,
        position: start,
    }]);
    let mut seen = HashSet::<Position>::new();

    while !nodes.is_empty() {
        let node = nodes.pop().unwrap();
        let position = node.position;
        if done(&node) {
            return node.steps;
        }
        if seen.contains(&position) {
            continue;
        }
        seen.insert(position);
        let neighbours = offsets.iter().flat_map(|(dx, dy)| {
            let x = (position.x as i32 - dx).try_into().ok()?;
            let y = (position.y as i32 - dy).try_into().ok()?;
            let height = grid.get((y, x))?;
            if *height <= node.height + 1 {
                Some((Position { x, y }, height))
            } else {
                None
            }
        });
        for (pos, &height) in neighbours {
            nodes.push(Node {
                steps: node.steps + 1,
                height,
                position: pos,
            });
        }
    }
    panic!("No path found");
}

pub fn solve(input: &str) -> (String, String) {
    let height_map = parse_input(input);
    let solution_a = shortest_path(&height_map.grid, height_map.start, |node| {
        node.position == height_map.end
    });
    let upside_down = 26 - height_map.grid;
    let solution_b = shortest_path(&upside_down, height_map.end, |node| node.height == 26);
    (solution_a.to_string(), solution_b.to_string())
}
