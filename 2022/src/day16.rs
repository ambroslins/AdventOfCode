use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{self, alpha1, line_ending},
    combinator::map,
    multi::separated_list1,
    sequence::{pair, preceded},
    IResult,
};

type Label = u32;

fn hash_label(input: &str) -> u32 {
    let p = 41;
    input
        .chars()
        .fold(0, |hash, char| hash * p + (char as u32 - 'A' as u32))
}

#[derive(Debug, Clone)]
struct Valve {
    flow: u32,
    tunnels: Vec<Label>,
}

fn parse_label(input: &str) -> IResult<&str, Label> {
    map(alpha1, hash_label)(input)
}

fn parse_valve(input: &str) -> IResult<&str, (Label, Valve)> {
    let label = preceded(tag("Valve "), parse_label);
    let flow_rate = preceded(tag(" has flow rate="), complete::u32);
    let parse_tunnels = preceded(
        alt((
            tag("; tunnels lead to valves "),
            tag("; tunnel leads to valve "),
        )),
        separated_list1(tag(", "), parse_label),
    );
    pair(
        label,
        map(pair(flow_rate, parse_tunnels), |(flow, tunnels)| Valve {
            flow,
            tunnels,
        }),
    )(input)
}

struct Network {
    valves: HashMap<Label, Valve>,
    shortest_path_cache: RefCell<HashMap<(Label, Label), Option<u32>>>,
}

impl Network {
    fn shortest_path(&self, source: &Label, target: &Label) -> Option<u32> {
        let mut cache = self.shortest_path_cache.borrow_mut();
        if let Some(depth) = cache
            .get(&(*source, *target))
            .or_else(|| cache.get(&(*target, *source)))
        {
            return *depth;
        }
        let mut seen = HashSet::<Label>::new();
        let mut queue = VecDeque::from([(source, 0)]);
        while let Some((&current, depth)) = queue.pop_front() {
            if current == *target {
                cache.insert((*source, *target), Some(depth));
                return Some(depth);
            }
            if !seen.insert(current) {
                continue;
            }
            for next in &self.valves.get(&current).unwrap().tunnels {
                queue.push_back((next, depth + 1));
            }
        }
        cache.insert((*source, *target), None);
        None
    }
}

#[derive(Debug, Clone)]
struct State {
    time: u32,
    current: Label,
    pressure: u32,
}

fn max_pressure_release(
    state: &State,
    elephant: Option<&State>,
    closed: &mut HashSet<Label>,
    network: &Network,
) -> u32 {
    let labels: Vec<Label> = Vec::from_iter(closed.iter().cloned());
    labels
        .iter()
        .flat_map(|next| {
            let distance = network.shortest_path(&state.current, next)?;
            let valve = network.valves.get(next)?;
            if distance >= state.time {
                return None;
            }
            let time = state.time - distance - 1;
            closed.remove(next);
            let s = State {
                time,
                current: *next,
                pressure: state.pressure + time * valve.flow,
            };
            let pressure = max_pressure_release(&s, elephant, closed, network);
            closed.insert(*next);
            Some(pressure)
        })
        .max()
        .unwrap_or_else(|| {
            state.pressure
                + if let Some(s) = elephant {
                    max_pressure_release(s, None, closed, network)
                } else {
                    0
                }
        })
}

fn solve_a(network: &Network) -> u32 {
    let mut closed: HashSet<Label> = network
        .valves
        .iter()
        .filter_map(|(label, valve)| if valve.flow > 0 { Some(*label) } else { None })
        .collect();
    let state = State {
        time: 30,
        current: hash_label("AA"),
        pressure: 0,
    };
    max_pressure_release(&state, None, &mut closed, network)
}

fn solve_b(network: &Network) -> u32 {
    let mut closed: HashSet<Label> = network
        .valves
        .iter()
        .filter_map(|(label, valve)| if valve.flow > 0 { Some(*label) } else { None })
        .collect();
    let state = State {
        time: 26,
        current: hash_label("AA"),
        pressure: 0,
    };
    max_pressure_release(&state, Some(&state), &mut closed, network)
}
pub fn solve(input: &str) -> (String, String) {
    let valves: HashMap<Label, Valve> = separated_list1(line_ending, parse_valve)(input)
        .unwrap()
        .1
        .into_iter()
        .collect();
    let network = Network {
        valves,
        shortest_path_cache: RefCell::default(),
    };
    (solve_a(&network).to_string(), solve_b(&network).to_string())
}
