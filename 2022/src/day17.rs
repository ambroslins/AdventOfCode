use std::{
    collections::{HashMap, VecDeque},
    ops::ControlFlow,
};

#[derive(Debug, Clone, Copy)]
enum Jet {
    Left,
    Right,
}

fn char_to_jet(char: char) -> Jet {
    match char {
        '<' => Jet::Left,
        '>' => Jet::Right,
        _ => panic!("invalid char: {char}"),
    }
}

type Row = u8;

#[derive(Debug, Clone)]
struct Chamber {
    rows: VecDeque<Row>,
    y: i64,
}

impl Chamber {
    fn height(&self) -> i64 {
        self.rows.len() as i64 + self.y
    }
}

type Shape = [Row; 4];

#[derive(Debug, Clone, Copy)]
struct Rock {
    rows: Shape,
    y: i64,
}

impl Rock {
    fn push(&self, jet: Jet) -> Option<Self> {
        match jet {
            Jet::Left if self.rows.iter().all(|&r| (r & 0b1000000) == 0) => Some(Rock {
                rows: self.rows.map(|r| r << 1),
                y: self.y,
            }),
            Jet::Right if self.rows.iter().all(|&r| (r & 0b1) == 0) => Some(Rock {
                rows: self.rows.map(|r| r >> 1),
                y: self.y,
            }),
            _ => None,
        }
    }

    fn collides(&self, chamber: &Chamber) -> bool {
        if self.y < 0 {
            return true;
        }
        let y = (self.y - chamber.y) as usize;
        self.rows.iter().enumerate().any(|(dy, a)| {
            if let Some(b) = chamber.rows.get(y + dy) {
                (a & b) != 0
            } else {
                false
            }
        })
    }

    fn drop<'a>(&self, chamber: &Chamber, jets: &mut impl Iterator<Item = &'a Jet>) -> Self {
        let stopped: ControlFlow<Self, Self> = jets.try_fold(*self, |rock, &jet| {
            let pushed = rock
                .push(jet)
                .filter(|r| !r.collides(chamber))
                .unwrap_or(rock);
            let dropped = Self {
                rows: pushed.rows,
                y: pushed.y - 1,
            };

            if dropped.collides(chamber) {
                ControlFlow::Break(pushed)
            } else {
                ControlFlow::Continue(dropped)
            }
        });
        match stopped {
            ControlFlow::Break(r) => r,
            ControlFlow::Continue(_) => panic!("impossible"),
        }
    }
}

fn simulate(shapes: &[Shape], jets: &[Jet], steps: u64) -> i64 {
    let mut chamber = Chamber {
        rows: VecDeque::new(),
        y: 0,
    };
    let mut jet_iter = jets.iter().cycle();
    let mut shape_iter = shapes.iter().cycle();
    let mut cache = HashMap::<(Vec<Row>, [Row; 4]), (u64, i64)>::new();
    let mut step = 0;
    while step < steps {
        let shape = shape_iter.next().unwrap();
        let cache_key = (Vec::from_iter(chamber.rows.iter().copied()), *shape);
        if let Some((s, height)) = cache.get(&cache_key) {
            let d_step = step - s;
            let d_height = chamber.height() - height;
            let skips = (steps - step) / d_step;
            step += d_step * skips;
            chamber.y += d_height * skips as i64;
            if step == steps {
                break;
            }
        }
        cache.insert(cache_key, (step, chamber.height()));
        step += 1;
        let rock = Rock {
            rows: *shape,
            y: chamber.height() + 3,
        };
        let stopped = rock.drop(&chamber, &mut jet_iter);
        let y = (stopped.y - chamber.y) as usize;
        for (dy, &row) in stopped.rows.iter().enumerate() {
            if row == 0 {
                break;
            }
            if let Some(r) = chamber.rows.get_mut(y + dy) {
                *r |= row;
            } else {
                chamber.rows.push_back(row);
            }
        }

        while chamber.rows.len() > 100 {
            chamber.rows.pop_front();
            chamber.y += 1;
        }
    }
    chamber.y + chamber.rows.len() as i64
}

pub fn solve(input: &str) -> (String, String) {
    let shapes = [
        [0b0011110, 0, 0, 0],
        [0b0001000, 0b0011100, 0b0001000, 0],
        [0b0011100, 0b0000100, 0b0000100, 0],
        [0b0010000, 0b0010000, 0b0010000, 0b0010000],
        [0b0011000, 0b0011000, 0, 0],
    ];
    let jets: Vec<Jet> = input.chars().map(char_to_jet).collect();
    (
        simulate(&shapes, &jets, 2022).to_string(),
        simulate(&shapes, &jets, 1000000000000).to_string(),
    )
}
