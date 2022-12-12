use ndarray::{arr1, Array1};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    combinator::{map, map_res},
    error::ParseError,
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, terminated, tuple},
    Err, IResult, Parser,
};
use std::{collections::VecDeque, str::FromStr};

type Level = Array1<u32>;

#[derive(Debug, Clone)]
struct Monkey {
    items: VecDeque<Level>,
    operation: Operation,
    test: u32,
    if_true: usize,
    if_false: usize,
    inspects: usize,
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Plus,
    Times,
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Lit(u32),
    Old,
}

#[derive(Debug, Clone, Copy)]
struct Operation {
    op: Op,
    value: Value,
}

impl Operation {
    fn apply(&self, value: Level) -> Level {
        let rhs = match self.value {
            Value::Lit(x) => arr1(&[x; 8]),
            Value::Old => value.clone(),
        };
        match self.op {
            Op::Plus => value + rhs,
            Op::Times => value * rhs,
        }
    }
}

fn lexeme<'a, F: Parser<&'a str, O, E>, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> Result<(&'a str, O), Err<E>> {
    terminated(inner, multispace0)
}

pub fn parse_number<I: FromStr>(input: &str) -> IResult<&str, I> {
    map_res(digit1, I::from_str)(input)
}

fn parse_starting_items(input: &str) -> IResult<&str, VecDeque<Level>> {
    lexeme(preceded(
        tag("Starting items: "),
        map(separated_list1(tag(", "), parse_number), |items| {
            items.iter().map(|&item| arr1(&[item; 8])).collect()
        }),
    ))(input)
}

fn parse_operation(input: &str) -> IResult<&str, Operation> {
    let prefix = tag("Operation: new = old ");
    let plus = map(tag("+ "), |_| Op::Plus);
    let times = map(tag("* "), |_| Op::Times);
    let op = alt((plus, times));
    let value = alt((
        map(tag("old"), |_| Value::Old),
        map(parse_number, Value::Lit),
    ));
    lexeme(preceded(
        prefix,
        map(tuple((op, value)), |(op, value)| Operation { op, value }),
    ))(input)
}

fn parse_test(input: &str) -> IResult<&str, (u32, usize, usize)> {
    let test = preceded(tag("Test: divisible by "), parse_number);
    let if_true = preceded(tag("If true: throw to monkey "), parse_number);
    let if_false = preceded(tag("If false: throw to monkey "), parse_number);
    tuple((lexeme(test), lexeme(if_true), lexeme(if_false)))(input)
}

fn parse_monkey(input: &str) -> IResult<&str, Monkey> {
    lexeme(preceded(
        lexeme(delimited(tag("Monkey "), parse_number::<usize>, tag(":"))),
        map(
            tuple((parse_starting_items, parse_operation, parse_test)),
            |(items, operation, (test, if_true, if_false))| Monkey {
                items,
                operation,
                test,
                if_true,
                if_false,
                inspects: 0,
            },
        ),
    ))(input)
}

fn play_round(monkeys: &mut Vec<Monkey>, divide_by: u32, primes: &Level) {
    for i in 0..monkeys.len() {
        while !monkeys[i].items.is_empty() {
            let monkey = &mut monkeys[i];
            monkey.inspects += 1;
            let item = monkey.items.pop_front().unwrap();
            let worry_level = monkey.operation.apply(item) % primes;
            let to = if worry_level[i] == 0 {
                monkey.if_true
            } else {
                monkey.if_false
            };
            monkeys[to].items.push_back(worry_level);
        }
    }
}

fn solve_a(mut monkeys: Vec<Monkey>, n: &Level) -> usize {
    for _ in 0..20 {
        play_round(&mut monkeys, 3, n);
    }
    let mut inspects: Vec<usize> = monkeys.iter().map(|monkey| monkey.inspects).collect();
    inspects.sort_by(|a, b| b.cmp(a));
    inspects.iter().take(2).product()
}

fn solve_b(mut monkeys: Vec<Monkey>, n: &Level) -> usize {
    for _ in 0..10000 {
        play_round(&mut monkeys, 1, n);
    }
    let mut inspects: Vec<usize> = monkeys.iter().map(|monkey| monkey.inspects).collect();
    println!("{inspects:?}");
    inspects.sort_by(|a, b| b.cmp(a));
    inspects.iter().take(2).product()
}

pub fn solve(input: &str) -> (String, String) {
    let (_, monkeys) = many0(parse_monkey)(input).unwrap();
    let primes: Array1<u32> = monkeys.iter().map(|monkey| monkey.test).collect();

    (
        solve_a(monkeys.clone(), &primes).to_string(),
        solve_b(monkeys, &primes).to_string(),
    )
}
