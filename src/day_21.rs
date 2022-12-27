use std::{
    fs::File,
    io::{BufRead, BufReader},
    mem,
};

pub fn solve(input: BufReader<File>) {
    let mut monkeys = Vec::new();

    for line in input.lines() {
        let line = line.unwrap();
        let (lhs, rhs) = line.trim().split_once(": ").unwrap();

        if let Ok(value) = rhs.parse::<u64>() {
            monkeys.push((lhs.to_owned(), Monkey::Value(value)));
        } else {
            let mut parts = rhs.split_whitespace();
            let left = parts.next().unwrap();
            let operator = parts.next().unwrap();
            let right = parts.next().unwrap();

            let operator = match operator {
                "+" => Operator::Add,
                "-" => Operator::Sub,
                "*" => Operator::Mul,
                "/" => Operator::Div,
                _ => panic!(),
            };

            monkeys.push((
                lhs.to_owned(),
                Monkey::Op(
                    Operand::Monkey(left.to_owned()),
                    operator,
                    Operand::Monkey(right.to_owned()),
                ),
            ));
        }
    }

    println!("root = {}", resolve_monkey("root", &mut monkeys).unwrap());
}

#[derive(Debug, Clone)]
enum Monkey {
    Value(u64),
    Op(Operand, Operator, Operand),
}

impl Monkey {
    fn value(&self) -> Option<u64> {
        match self {
            Self::Value(value) => Some(*value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Operand {
    Value(u64),
    Monkey(String),
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operator {
    fn eval(self, lhs: u64, rhs: u64) -> u64 {
        match self {
            Self::Add => lhs + rhs,
            Self::Sub => lhs - rhs,
            Self::Mul => lhs * rhs,
            Self::Div => lhs / rhs,
        }
    }
}

fn resolve_monkey(
    name: &str,
    monkeys: &mut Vec<(String, Monkey)>,
) -> Option<u64> {
    let known_values = monkeys
        .iter()
        .filter_map(|(name, monkey)| monkey.value().map(|v| (name.clone(), v)))
        .collect::<Vec<_>>();

    resolve_monkey_(name, mem::take(monkeys), known_values)
}

fn resolve_monkey_(
    root_name: &str,
    mut monkeys: Vec<(String, Monkey)>,
    mut known_values: Vec<(String, u64)>,
) -> Option<u64> {
    for (name, value) in mem::take(&mut known_values) {
        for (other_name, monkey) in mem::take(&mut monkeys) {
            match monkey {
                Monkey::Op(
                    Operand::Monkey(m),
                    op,
                    rhs @ Operand::Monkey(_),
                ) if m == name => {
                    monkeys.push((
                        other_name,
                        Monkey::Op(Operand::Value(value), op, rhs),
                    ));
                }
                Monkey::Op(
                    lhs @ Operand::Monkey(_),
                    op,
                    Operand::Monkey(m),
                ) if m == name => {
                    monkeys.push((
                        other_name,
                        Monkey::Op(lhs, op, Operand::Value(value)),
                    ));
                }
                Monkey::Op(Operand::Monkey(m), op, Operand::Value(rhs))
                    if m == name =>
                {
                    let value = op.eval(value, rhs);
                    if other_name == root_name {
                        return Some(value);
                    }

                    known_values.push((other_name.clone(), value));
                }
                Monkey::Op(Operand::Value(lhs), op, Operand::Monkey(m))
                    if m == name =>
                {
                    let value = op.eval(lhs, value);
                    if other_name == root_name {
                        return Some(value);
                    }

                    known_values.push((other_name.clone(), value));
                }
                Monkey::Value(_) => (),
                m => monkeys.push((other_name, m)),
            }
        }
    }

    if !known_values.is_empty() {
        resolve_monkey_(root_name, monkeys, known_values)
    } else {
        None
    }
}
