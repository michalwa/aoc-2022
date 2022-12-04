use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub struct Day3;

impl Day for Day3 {
    fn solve(&self, input: BufReader<File>) {
        let priority_sum = input
            .lines()
            .map(Result::unwrap)
            // Part 1
            // .map(|line| {
            //     let (a, b) = line.split_at(line.len() / 2);
            //     [a.to_owned(), b.to_owned()]
            // })
            // Part 2
            .array_chunks::<3>()
            // Both
            .map(|lines| {
                lines
                    .into_iter()
                    .map(|lines| {
                        lines
                            .bytes()
                            .map(|b| 1u64 << priority(b))
                            .fold(0, |a, b| a | b)
                    })
                    .reduce(|a, b| a & b)
                    .unwrap()
            })
            .map(sum_priorities)
            .sum::<u32>();

        println!("{priority_sum}");
    }
}

fn priority(byte: u8) -> u8 {
    match byte {
        b'a'..=b'z' => byte - b'a' + 1,
        b'A'..=b'Z' => byte - b'A' + 27,
        _ => panic!("bad rucksack item"),
    }
}

fn sum_priorities(bitset: u64) -> u32 {
    (0..64).filter(|i| bitset & (1 << i) != 0).sum()
}
