use std::collections::HashSet;

use ndarray::Array;
use ndarray::Array2;
use ndarray::ArrayView1;
use ndarray::Axis;
use ndarray::Ix2;

fn solve_a(trees: &Array2<i32>) -> usize {
    let mut visible: HashSet<Ix2> = HashSet::new();
    fn scan(visible: &mut HashSet<Ix2>, mut view: impl Iterator<Item = (i32, Ix2)>) {
        view.try_fold(-1, |max_height, (current_height, ix)| {
            if current_height > max_height {
                visible.insert(ix);
                if current_height == 9 {
                    return None;
                } else {
                    return Some(current_height);
                };
            }
            Some(max_height)
        });
    }

    for (y, row) in trees.rows().into_iter().enumerate() {
        let iter = row
            .iter()
            .enumerate()
            .map(|(x, height)| (*height, Ix2(x, y)));
        scan(&mut visible, iter.clone());
        scan(&mut visible, iter.rev());
    }

    for (x, col) in trees.columns().into_iter().enumerate() {
        let iter = col
            .iter()
            .enumerate()
            .map(|(y, height)| (*height, Ix2(x, y)));
        scan(&mut visible, iter.clone());
        scan(&mut visible, iter.rev());
    }

    visible.len()
}

fn solve_b(trees: &Array2<i32>) -> usize {
    let mut best_score: usize = 0;
    fn walk_while_smaller<'a>(height: i32, iter: impl Iterator<Item = &'a i32>) -> usize {
        let mut steps: usize = 0;
        for tree in iter {
            steps += 1;
            if tree >= &height {
                break;
            }
        }
        steps
    }
    fn lane_score(index: usize, view: ArrayView1<i32>) -> usize {
        let &tree_height = view.get(index).unwrap();
        let (front, back) = view.split_at(Axis(0), index);
        let score_front = walk_while_smaller(tree_height, front.into_iter().rev());
        let score_back = walk_while_smaller(tree_height, back.into_iter().skip(1));
        score_front * score_back
    }
    for (x, col) in trees.columns().into_iter().enumerate() {
        for (y, row) in trees.rows().into_iter().enumerate() {
            let score = lane_score(y, col) * lane_score(x, row);
            best_score = std::cmp::max(score, best_score);
        }
    }
    best_score
}

pub fn solve(input: &str) -> (String, String) {
    let width: usize = input.lines().next().unwrap().len();
    let height: usize = input.lines().count();
    let trees: Array2<i32> = Array::from_shape_vec(
        (height, width),
        input
            .lines()
            .flat_map(|line| line.chars())
            .map(|char| char.to_digit(10).unwrap().try_into().unwrap())
            .collect(),
    )
    .unwrap();
    (solve_a(&trees).to_string(), solve_b(&trees).to_string())
}
