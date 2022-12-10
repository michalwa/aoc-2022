use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub fn solve(input: BufReader<File>) {
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
