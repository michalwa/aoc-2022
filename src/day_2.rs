use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub struct Day2;

impl Day for Day2 {
    fn solve(&self, input: BufReader<File>) {
        let total_score = input
            .lines()
            .map(|line| {
                let line = line.unwrap();
                let line = line.trim().as_bytes();
                let opponent = rock_paper_scissors(line[0]);

                // Part 1
                // let player = rock_paper_scissors(line[2]);

                // Part 2
                let player = match line[2] {
                    b'X' => (opponent + 2) % 3,
                    b'Y' => opponent,
                    b'Z' => (opponent + 1) % 3,
                    _ => panic!("unsupported byte"),
                };

                score(player, opponent) + player as u32 + 1
            })
            .sum::<u32>();

        println!("{total_score}");
    }
}

fn rock_paper_scissors(byte: u8) -> u8 {
    match byte {
        b'A' | b'X' => 0,
        b'B' | b'Y' => 1,
        b'C' | b'Z' => 2,
        _ => panic!("unsupported byte"),
    }
}

fn score(player: u8, opponent: u8) -> u32 {
    match () {
        _ if opponent == (player + 2) % 3 => 6,
        _ if opponent == player => 3,
        _ => 0,
    }
}
