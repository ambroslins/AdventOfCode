use std::collections::HashMap;

use ndarray::{arr1, Array1, Array2, ArrayView1, Zip};
use nom::{
    bytes::complete::tag,
    character::complete::{self, alpha1, line_ending, space0, space1},
    combinator::{map, map_res},
    multi::separated_list1,
    sequence::{delimited, pair, terminated},
    IResult,
};

type Resource = usize;

#[derive(Debug, Clone)]
struct Blueprint {
    id: usize,
    robots: Array2<Resource>,
}

fn parse_resource(input: &str) -> IResult<&str, Resource> {
    map_res(alpha1, |resource| match resource {
        "ore" => Ok(0),
        "clay" => Ok(1),
        "obsidian" => Ok(2),
        "geode" => Ok(3),
        _ => Err(format!("invalid resource: {resource}")),
    })(input)
}

fn parse_usize(input: &str) -> IResult<&str, usize> {
    map(complete::u32, |u| u as usize)(input)
}

fn parse_cost(input: &str) -> IResult<&str, (Resource, usize)> {
    map(
        pair(terminated(parse_usize, space0), parse_resource),
        |(cost, resource)| (resource, cost),
    )(input)
}

fn parse_robot(input: &str) -> IResult<&str, (Resource, Array1<usize>)> {
    pair(
        delimited(tag("Each "), parse_resource, tag(" robot")),
        map(
            delimited(
                tag(" costs "),
                separated_list1(tag(" and "), parse_cost),
                tag("."),
            ),
            |costs| {
                let mut cost_array = Array1::zeros(4);
                for (res, cost) in &costs {
                    cost_array[*res] = *cost;
                }
                cost_array
            },
        ),
    )(input)
}

fn parse_blueprint(input: &str) -> IResult<&str, Blueprint> {
    let id = delimited(tag("Blueprint "), parse_usize, tag(": "));
    let robots = map(separated_list1(space1, parse_robot), |rs| {
        let mut robots = Array2::zeros((4, 4));
        for (res, costs) in rs {
            robots.row_mut(res).assign(&costs);
        }
        robots
    });
    map(pair(id, robots), |(id, robots)| Blueprint { id, robots })(input)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    time: usize,
    resources: Array1<usize>,
    robots: Array1<usize>,
}

fn maximize_geodes(blueprint: &Blueprint, state: &State) -> usize {
    (0..4)
        .flat_map(|resource| {
            let res_costs = blueprint.robots.column(resource);
            let most_expensive = res_costs.iter().max().unwrap();
            if resource != 3 && state.robots[resource] >= *most_expensive {
                return None;
            }
            let costs = blueprint.robots.row(resource);
            let dt = 1 + state
                .resources
                .iter()
                .zip(state.robots.iter())
                .zip(costs.iter())
                .map(|((res, rs), cost)| {
                    if res >= cost {
                        Some(0)
                    } else if *rs != 0 {
                        Some((cost - res).div_ceil(*rs))
                    } else {
                        None
                    }
                })
                .collect::<Option<Vec<usize>>>()
                .and_then(|dts| dts.iter().max().copied())?;
            if dt > state.time {
                return None;
            }
            let mut robots = state.robots.clone();
            robots[resource] += 1;
            let s = State {
                time: state.time - dt,
                resources: &state.resources + &state.robots * dt - costs,
                robots,
            };
            Some(maximize_geodes(blueprint, &s))
        })
        .max()
        .unwrap_or_else(|| {
            *(&state.resources + &state.robots * state.time)
                .last()
                .unwrap()
        })
}

fn solve_a(blueprints: &[Blueprint]) -> usize {
    let state = State {
        time: 24,
        resources: Array1::zeros(4),
        robots: arr1(&[1, 0, 0, 0]),
    };
    blueprints
        .iter()
        .map(|blueprint| {
            println!("Solving {}", blueprint.id);
            let max = maximize_geodes(blueprint, &state);
            println!("Best: {max}");
            blueprint.id * max
        })
        .sum()
}

fn solve_b(blueprints: &[Blueprint]) -> usize {
    let state = State {
        time: 32,
        resources: Array1::zeros(4),
        robots: arr1(&[1, 0, 0, 0]),
    };
    blueprints
        .iter()
        .take(3)
        .map(|blueprint| {
            println!("Solving {}", blueprint.id);
            let max = maximize_geodes(blueprint, &state);
            println!("Best: {max}");
            max
        })
        .product()
}

pub fn solve(input: &str) -> (String, String) {
    let mut parser = separated_list1(line_ending, parse_blueprint);
    let (_, blueprints) = parser(input).unwrap();
    println!("{blueprints:?}");
    (solve_b(&blueprints).to_string(), "".to_owned())
}
