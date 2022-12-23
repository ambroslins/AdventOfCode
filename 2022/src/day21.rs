use core::panic;
use std::collections::HashMap;

use nom::{
    branch::alt,
    character::{
        complete::{self, alpha1, anychar, line_ending, space1},
        streaming::char,
    },
    combinator::{map, map_res},
    multi::separated_list1,
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone)]
enum Expr {
    Lit(i64),
    Var(String),
    App(Operator, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
}

fn parse_name(input: &str) -> IResult<&str, String> {
    map(alpha1, |name: &str| name.to_string())(input)
}

fn parse_operator(input: &str) -> IResult<&str, Operator> {
    map_res(anychar, |op| match op {
        '+' => Ok(Operator::Plus),
        '-' => Ok(Operator::Minus),
        '*' => Ok(Operator::Times),
        '/' => Ok(Operator::Divide),
        _ => Err(()),
    })(input)
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        map(complete::i64, Expr::Lit),
        map(
            tuple((
                map(parse_name, Expr::Var),
                delimited(space1, parse_operator, space1),
                map(parse_name, Expr::Var),
            )),
            |(lhs, op, rhs)| Expr::App(op, Box::new(lhs), Box::new(rhs)),
        ),
    ))(input)
}

fn parse_declaration(input: &str) -> IResult<&str, (String, Expr)> {
    let annotation = terminated(char(':'), space1);
    separated_pair(parse_name, annotation, parse_expr)(input)
}

impl Expr {
    fn eval(&self, context: &HashMap<String, Expr>) -> Option<i64> {
        match &self {
            Expr::Lit(x) => Some(*x),
            Expr::Var(var) => context.get(var).and_then(|expr| expr.eval(context)),
            Expr::App(op, lhs, rhs) => {
                let x = lhs.eval(context)?;
                let y = rhs.eval(context)?;
                Some(match op {
                    Operator::Plus => x + y,
                    Operator::Minus => x - y,
                    Operator::Times => x * y,
                    Operator::Divide => x / y,
                })
            }
        }
    }

    fn solve(&self, variable: &String, result: i64, context: &HashMap<String, Expr>) -> i64 {
        match &self {
            Expr::Lit(x) => {
                if *x == result {
                    0
                } else {
                    panic!("solver failed");
                }
            }
            Expr::Var(var) => {
                if var == variable {
                    result
                } else {
                    context.get(var).unwrap().solve(variable, result, context)
                }
            }
            Expr::App(op, lhs, rhs) => match (lhs.eval(context), rhs.eval(context)) {
                (None, None) => panic!("both None"),
                (None, Some(y)) => {
                    let x = match op {
                        Operator::Plus => result - y,
                        Operator::Minus => result + y,
                        Operator::Times => result / y,
                        Operator::Divide => result * y,
                    };
                    lhs.solve(variable, x, context)
                }
                (Some(x), None) => {
                    let y = match op {
                        Operator::Plus => result - x,
                        Operator::Minus => x - result,
                        Operator::Times => result / x,
                        Operator::Divide => x / result,
                    };
                    rhs.solve(variable, y, context)
                }
                (Some(_), Some(_)) => panic!("both Some"),
            },
        }
    }
}

fn solve_b(mut context: HashMap<String, Expr>) -> i64 {
    let human = "humn".to_owned();
    context.remove(&human);
    let Expr::App(_, lhs, rhs) = context.remove(&"root".to_owned()).unwrap() else { panic!("bad root")};
    Expr::App(Operator::Minus, lhs, rhs).solve(&human, 0, &context)
}

pub fn solve(input: &str) -> (String, String) {
    let context: HashMap<String, Expr> = separated_list1(line_ending, parse_declaration)(input)
        .unwrap()
        .1
        .into_iter()
        .collect();
    println!("{context:?}");
    let solution_a = Expr::Var("root".to_owned()).eval(&context).unwrap();

    (solution_a.to_string(), solve_b(context).to_string())
}
