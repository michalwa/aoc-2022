use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub struct Day1;

impl Day for Day1 {
    fn solve(&self, input: BufReader<File>) {
        let values = input
            .lines()
            .map(|line| line.unwrap().trim().parse::<u32>().ok())
            .collect::<Vec<_>>();

        let sums = values
            .split(Option::is_none)
            .map(|group| group.iter().cloned().map(Option::unwrap).sum::<u32>());

        // Part 1
        // println!("{:?}", sums.max());

        let mut sums = sums.collect::<Vec<_>>();
        sums.sort();
        sums.reverse();
        println!("{:?}", sums[..3].iter().sum::<u32>());
    }
}
