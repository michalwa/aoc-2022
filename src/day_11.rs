use std::{
    fs::File,
    io::{BufReader, Read},
    mem,
};

const NUM_ROUNDS: usize = 10_000; // Part 1: 20

pub fn solve(mut input: BufReader<File>) {
    let mut contents = String::new();
    input.read_to_string(&mut contents).unwrap();

    let mut monkeys = parse_monkeys(&contents);

    // Common multiple of `test_divisor`-s, we can get away with just multiplying
    // them because they're small and because in the inputs they're always primes anyway.
    let modulus = monkeys.iter().map(|m| m.test_divisor).product::<u64>();

    for _ in 0..NUM_ROUNDS {
        for i in 0..monkeys.len() {
            monkeys[i].num_inspections += monkeys[i].items.len();

            for item in mem::take(&mut monkeys[i].items) {
                let Monkey {
                    operation,
                    test_divisor,
                    monkey_if_true,
                    monkey_if_false,
                    ..
                } = monkeys[i];

                // Part 1
                // let worry = operation.eval(item) / 3;
                let worry = operation.eval(item) % modulus;

                if worry % test_divisor == 0 {
                    monkeys[monkey_if_true].items.push(worry);
                } else {
                    monkeys[monkey_if_false].items.push(worry);
                }
            }
        }
    }

    monkeys.sort_unstable_by_key(|m| usize::MAX - m.num_inspections);
    let monkey_business = monkeys[..2]
        .iter()
        .map(|m| m.num_inspections)
        .product::<usize>();

    println!("{monkey_business}");
}

#[derive(Debug)]
struct Monkey {
    items: Vec<u64>,
    operation: Operation,
    test_divisor: u64,
    monkey_if_true: usize,
    monkey_if_false: usize,
    num_inspections: usize,
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Add(Operand, Operand),
    Mul(Operand, Operand),
}

#[derive(Debug, Clone, Copy)]
enum Operand {
    Old,
    Const(u64),
}

fn parse_monkeys(contents: &str) -> Vec<Monkey> {
    contents
        .split("\n\n")
        .map(|monkey_def| {
            let lines = monkey_def.lines().skip(1).collect::<Vec<_>>();

            let items = trim_prefix("Starting items: ", lines[0])
                .split(", ")
                .map(|num| num.parse::<u64>())
                .collect::<Result<Vec<_>, _>>()
                .unwrap();

            let mut operation_tokens =
                trim_prefix("Operation: new = ", lines[1]).split_whitespace();

            let lhs =
                Operand::from_str(operation_tokens.next().unwrap()).unwrap();
            let operator = operation_tokens.next().unwrap();
            let rhs =
                Operand::from_str(operation_tokens.next().unwrap()).unwrap();
            let operation = Operation::from_items(lhs, operator, rhs).unwrap();

            let test_divisor = trim_prefix("Test: divisible by ", lines[2])
                .parse::<u64>()
                .unwrap();

            let monkey_if_true =
                trim_prefix("If true: throw to monkey ", lines[3])
                    .parse::<usize>()
                    .unwrap();

            let monkey_if_false =
                trim_prefix("If false: throw to monkey ", lines[4])
                    .parse::<usize>()
                    .unwrap();

            Monkey {
                items,
                operation,
                test_divisor,
                monkey_if_true,
                monkey_if_false,
                num_inspections: 0,
            }
        })
        .collect()
}

fn trim_prefix<'a>(prefix: &str, s: &'a str) -> &'a str {
    s.trim().strip_prefix(prefix).unwrap()
}

impl Operand {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "old" => Some(Self::Old),
            s => Some(Self::Const(s.parse().ok()?)),
        }
    }

    fn eval(self, old: u64) -> u64 {
        match self {
            Self::Old => old,
            Self::Const(c) => c,
        }
    }
}

impl Operation {
    fn from_items(lhs: Operand, operator: &str, rhs: Operand) -> Option<Self> {
        match operator {
            "+" => Some(Self::Add(lhs, rhs)),
            "*" => Some(Self::Mul(lhs, rhs)),
            _ => None,
        }
    }

    fn eval(self, old: u64) -> u64 {
        match self {
            Self::Add(lhs, rhs) => lhs.eval(old) + rhs.eval(old),
            Self::Mul(lhs, rhs) => lhs.eval(old) * rhs.eval(old),
        }
    }
}
