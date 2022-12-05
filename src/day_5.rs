use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub struct Day5;

impl Day for Day5 {
    fn solve(&self, input: BufReader<File>) {
        let lines = input.lines().map(Result::unwrap).collect::<Vec<_>>();
        let mut sections = lines.split(|line| line.trim().is_empty());
        let stack_rows = sections.next().unwrap();
        let instructions = sections.next().unwrap();

        let mut stacks = Vec::new();

        for row in &stack_rows[..stack_rows.len() - 1] {
            let items = row
                .as_bytes()
                .chunks(4)
                .map(|item| item[1] as char)
                .collect::<Vec<_>>();

            stacks.resize_with(stacks.len().max(items.len()), Vec::new);

            for (item, stack) in items.into_iter().zip(&mut stacks) {
                if !item.is_whitespace() {
                    stack.push(item);
                }
            }
        }

        for stack in &mut stacks {
            stack.reverse();
        }

        for instruction in instructions {
            let parts = instruction.split_whitespace().collect::<Vec<_>>();
            let [n_items, from, to] = [parts[1], parts[3], parts[5]]
                .map(|p| p.parse::<usize>().unwrap());

            // Part 1
            // for _ in 0..n_items {
            //     let item = stacks[from - 1].pop().unwrap();
            //     stacks[to - 1].push(item);
            // }

            // Part 2
            // FIXME: Figure out a way to borrow both stacks mutably
            let from_stack = &mut stacks[from - 1];
            let items = from_stack.drain((from_stack.len() - n_items)..).collect::<Vec<_>>();
            stacks[to - 1].extend(items);
        }

        println!(
            "{}",
            stacks
                .into_iter()
                .map(|s| *s.last().unwrap())
                .collect::<String>()
        );
    }
}
