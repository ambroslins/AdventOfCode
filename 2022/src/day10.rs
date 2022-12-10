use std::iter;
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
enum Instruction {
    AddX(i32),
    Noop,
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split(' ').collect::<Vec<_>>()[..] {
            ["addx", value] => Ok(Instruction::AddX(value.parse().unwrap())),
            ["noop"] => Ok(Instruction::Noop),
            _ => Err("no match".to_owned()),
        }
    }
}

fn solve_a(cycles: &[i32]) -> i32 {
    cycles
        .iter()
        .enumerate()
        .skip(19)
        .step_by(40)
        .take(6)
        .map(|(cycle, value)| ((cycle as i32) + 1) * value)
        .sum()
}

fn solve_b(cycles: &[i32]) -> String {
    cycles
        .chunks(40)
        .flat_map(|line| {
            line.iter()
                .enumerate()
                .map(|(index, &value)| {
                    if [value - 1, value, value + 1].contains(&(index as i32)) {
                        '#'
                    } else {
                        '.'
                    }
                })
                .chain(iter::once('\n'))
        })
        .collect()
}

pub fn solve(input: &str) -> (String, String) {
    let mut x: i32 = 1;
    let cycles: Vec<i32> = input
        .lines()
        .map(|line| line.parse().unwrap())
        .flat_map(|instruction| match instruction {
            Instruction::AddX(value) => {
                let result = vec![x, x];
                x += value;
                result
            }
            Instruction::Noop => vec![x],
        })
        .collect();
    (solve_a(&cycles).to_string(), solve_b(&cycles))
}
