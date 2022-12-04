use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    ops::RangeInclusive,
};

pub struct Day4;

impl Day for Day4 {
    fn solve(&self, input: BufReader<File>) {
        let count = input
            .lines()
            .map(Result::unwrap)
            .map(|line| {
                let (a, b) = line.trim().split_once(',').unwrap();
                (parse_range(a).unwrap(), parse_range(b).unwrap())
            })
            // Part 1
            // .filter(|(a, b)| is_subrange(a, b) || is_subrange(b, a))
            // Part 2
            .filter(|(a, b)| is_overlapping(a, b))
            .count();

        println!("{count}");
    }
}

fn parse_range(s: &str) -> Option<RangeInclusive<u32>> {
    let (start, end) = s.split_once('-')?;
    Some(start.parse::<u32>().ok()?..=end.parse::<u32>().ok()?)
}

#[allow(dead_code)]
fn is_subrange(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
    a.start() >= b.start() && a.end() <= b.end()
}

fn is_overlapping(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
   a.start() <= b.end() && a.end() >= b.start()
}
