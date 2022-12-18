use std::{collections::VecDeque, ops::ControlFlow};

type Jet = i32;

fn char_to_jet(char: char) -> Jet {
    match char {
        '<' => -1,
        '>' => 1,
        _ => panic!("invalid char: {char}"),
    }
}

type Chamber = Vec<VecDeque<i32>>;

type Rock = Vec<(i32, i32)>;

fn move_rock(rock: &Rock, dx: i32, dy: i32) -> Rock {
    rock.iter().map(|(x, y)| (x + dx, y + dy)).collect()
}

fn rock_collides(chamber: &Chamber, rock: &Rock) -> bool {
    rock.iter().any(
        |(x, y)| match (*x).try_into().ok().and_then(|i: usize| chamber.get(i)) {
            Some(ys) => ys.contains(y),
            None => true,
        },
    )
}

fn try_move(chamber: &Chamber, rock: &Rock, dx: i32, dy: i32) -> Option<Rock> {
    Some(move_rock(rock, dx, dy)).filter(|r| !rock_collides(chamber, r))
}

fn drop_rock<'a>(chamber: &Chamber, rock: &Rock, jets: &mut impl Iterator<Item = &'a i32>) -> Rock {
    let stopped: ControlFlow<Rock, Rock> = jets.try_fold(rock.clone(), |r, &jet| {
        let pushed = try_move(chamber, &r, jet, 0).unwrap_or(r);
        if let Some(s) = try_move(chamber, &pushed, 0, -1) {
            ControlFlow::Continue(s)
        } else {
            ControlFlow::Break(pushed)
        }
    });
    match stopped {
        ControlFlow::Break(r) => r,
        ControlFlow::Continue(_) => panic!("impossible"),
    }
}

fn solve_a(rocks: &[Rock], jets: &[Jet]) -> i32 {
    let mut chamber = vec![VecDeque::from([0]); 7];
    let mut jet_iter = jets.iter().cycle();
    for rock in rocks.iter().cycle().take(1000000000000) {
        let max = chamber.iter().flat_map(|col| col.back()).max().unwrap();
        let stopped = drop_rock(&chamber, &move_rock(rock, 2, max + 4), &mut jet_iter);
        // println!("{stopped:?}");
        for (x, y) in stopped {
            chamber[x as usize].push_back(y);
        }
    }
    *chamber.iter().flat_map(|col| col.back()).max().unwrap()
}

pub fn solve(input: &str) -> (String, String) {
    let rocks = vec![
        vec![(0, 0), (1, 0), (2, 0), (3, 0)],
        vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
        vec![(0, 0), (0, 1), (0, 2), (0, 3)],
        vec![(0, 0), (1, 0), (0, 1), (1, 1)],
    ];
    let jets: Vec<Jet> = input.chars().map(char_to_jet).collect();
    (solve_a(&rocks, &jets).to_string(), "".to_owned())
}
