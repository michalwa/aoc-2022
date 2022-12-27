use std::{
    collections::HashSet,
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

    monkeys = resolve_monkeys(monkeys, Default::default());
    let root = monkeys
        .into_iter()
        .find(|(name, _)| name == "root")
        .unwrap()
        .1;

    println!("root = {}", root.value().unwrap());
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

fn resolve_monkeys(
    monkeys: Vec<(String, Monkey)>,
    mut resolved_monkeys: HashSet<String>,
) -> Vec<(String, Monkey)> {
    let monkeys_to_resolve = monkeys
        .iter()
        .filter(|(name, _)| !resolved_monkeys.contains(name.as_str()))
        .filter_map(|(name, monkey)| Some(name.clone()).zip(monkey.value()))
        .collect::<Vec<_>>();

    let mut result = monkeys;
    let mut run_again = false;

    for (name, value) in monkeys_to_resolve {
        let mut monkey_referenced = false;

        for (other_name, monkey) in mem::take(&mut result) {
            let new_monkey = match monkey {
                Monkey::Op(
                    Operand::Monkey(m),
                    op,
                    rhs @ Operand::Monkey(_),
                ) if m == name => {
                    monkey_referenced = true;
                    Monkey::Op(Operand::Value(value), op, rhs)
                }
                Monkey::Op(
                    lhs @ Operand::Monkey(_),
                    op,
                    Operand::Monkey(m),
                ) if m == name => {
                    monkey_referenced = true;
                    Monkey::Op(lhs, op, Operand::Value(value))
                }
                Monkey::Op(Operand::Monkey(m), op, Operand::Value(rhs))
                    if m == name =>
                {
                    monkey_referenced = true;
                    run_again = true;
                    Monkey::Value(op.eval(value, rhs))
                }
                Monkey::Op(Operand::Value(lhs), op, Operand::Monkey(m))
                    if m == name =>
                {
                    monkey_referenced = true;
                    run_again = true;
                    Monkey::Value(op.eval(lhs, value))
                }
                m => m,
            };

            result.push((other_name, new_monkey));
        }

        if !monkey_referenced {
            resolved_monkeys.insert(name);
        }
    }

    if run_again {
        resolve_monkeys(result, resolved_monkeys)
    } else {
        result
    }
}
