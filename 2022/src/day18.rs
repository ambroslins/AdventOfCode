use std::{
    cmp,
    collections::{HashSet, VecDeque},
    str::FromStr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Cube {
    x: i32,
    y: i32,
    z: i32,
}

impl Cube {
    fn sides(&self) -> Vec<Self> {
        [(1, 0, 0), (0, 1, 0), (0, 0, 1)]
            .iter()
            .flat_map(|(dx, dy, dz)| {
                [
                    Cube {
                        x: self.x + dx,
                        y: self.y + dy,
                        z: self.z + dz,
                    },
                    Cube {
                        x: self.x - dx,
                        y: self.y - dy,
                        z: self.z - dz,
                    },
                ]
            })
            .collect()
    }
}

impl FromStr for Cube {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let [sx, sy, sz]: [&str; 3] = s.split(',').collect::<Vec<_>>().try_into().unwrap();
        let x = sx.parse()?;
        let y = sy.parse()?;
        let z = sz.parse()?;
        Ok(Cube { x, y, z })
    }
}

fn find_pockets(
    cube: &Cube,
    cubes: &HashSet<Cube>,
    min: i32,
    max: i32,
) -> Result<HashSet<Cube>, HashSet<Cube>> {
    let mut pockets = HashSet::<Cube>::new();
    let mut queue = VecDeque::from([*cube]);
    while let Some(c) = queue.pop_front() {
        if [c.x, c.y, c.z].iter().any(|&a| a < min || a > max) {
            return Err(pockets);
        }
        if pockets.contains(&c) || cubes.contains(&c) {
            continue;
        }
        pockets.insert(c);
        queue.append(&mut VecDeque::from(c.sides()));
    }
    Ok(pockets)
}

fn solve_a(cubes: &HashSet<Cube>) -> usize {
    cubes
        .iter()
        .map(|cube| {
            cube.sides()
                .iter()
                .filter(|side| !cubes.contains(side))
                .count()
        })
        .sum()
}

fn solve_b(cubes: &HashSet<Cube>) -> usize {
    let (min, max) = cubes
        .iter()
        .flat_map(|cube| [cube.x, cube.y, cube.z])
        .fold((i32::MAX, i32::MIN), |(min, max), a| {
            (cmp::min(min, a), cmp::max(max, a))
        });
    let mut pockets = HashSet::<Cube>::new();
    let mut escape = HashSet::<Cube>::new();
    cubes
        .iter()
        .map(|cube| {
            cube.sides()
                .iter()
                .filter(|side| {
                    if cubes.contains(side) || pockets.contains(side) {
                        return false;
                    }
                    if escape.contains(side) {
                        return true;
                    }
                    match find_pockets(side, cubes, min, max) {
                        Err(cs) => {
                            escape.extend(cs.iter());
                            true
                        }
                        Ok(cs) => {
                            pockets.extend(cs.iter());
                            false
                        }
                    }
                })
                .count()
        })
        .sum()
}

pub fn solve(input: &str) -> (String, String) {
    let cubes: HashSet<Cube> = input.lines().map(|line| line.parse().unwrap()).collect();
    (solve_a(&cubes).to_string(), solve_b(&cubes).to_string())
}
