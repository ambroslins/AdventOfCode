use std::str::FromStr;

type Crate = char;
type Stack = Vec<Crate>;
type Stacks = Vec<Stack>;

fn parse_stacks(input: &str) -> Stacks {
    let mut lines = input.lines().rev();
    let indices: Vec<usize> = lines
        .next()
        .unwrap()
        .char_indices()
        .flat_map(|(i, c)| if c.is_numeric() { Some(i) } else { None })
        .collect();
    let mut stacks: Stacks = indices.iter().map(|_| Vec::new()).collect();
    for line in lines {
        let chars: Vec<char> = line.chars().collect();
        for (i, j) in indices.iter().enumerate() {
            if let Some(c) = chars.get(*j).filter(|c| c.is_alphabetic()) {
                stacks[i].push(*c);
            }
        }
    }
    stacks
}

#[derive(Debug, Clone)]
struct Step {
    quantity: usize,
    from: usize,
    to: usize,
}

impl FromStr for Step {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let values: Vec<usize> = s
            .split_whitespace()
            .flat_map(|word| word.parse::<usize>())
            .collect();
        match values[..] {
            [quantity, from, to] => Ok(Step { quantity, from, to }),
            _ => Err(()),
        }
    }
}

fn top_of_stacks(stacks: &Stacks) -> String {
    return stacks.iter().flat_map(|stack| stack.last()).collect();
}

pub fn solve(input: &str) -> (String, String) {
    let (drawing, instructions) = input.split_once("\r\n\r\n").unwrap();
    let starting_steps = parse_stacks(drawing);
    let steps: Vec<Step> = instructions
        .lines()
        .map(|line| line.parse::<Step>().unwrap())
        .collect();

    let solution_a: Stacks = steps
        .iter()
        .fold(starting_steps.clone(), |mut stacks, step| {
            for _ in 0..step.quantity {
                let c = stacks[step.from - 1].pop().unwrap();
                stacks[step.to - 1].push(c);
            }
            stacks
        });

    let solution_b: Stacks = steps.iter().fold(starting_steps, |mut stacks, step| {
        let from = &mut stacks[step.from - 1];
        let mut crates: Stack = from.splice((from.len() - step.quantity).., []).collect();
        stacks[step.to - 1].append(&mut crates);
        stacks
    });

    (top_of_stacks(&solution_a), top_of_stacks(&solution_b))
}
