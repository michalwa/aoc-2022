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
                let opponent = RockPaperScissors::from_byte(line[0]);

                // Part 1
                // let player = RockPaperScissors::from_byte(line[2]);

                // Part 2
                let player = match line[2] {
                    b'X' => RockPaperScissors::losing_against(opponent),
                    b'Y' => opponent,
                    b'Z' => RockPaperScissors::winning_against(opponent),
                    _ => panic!("unsupported byte"),
                };

                player.score_against(opponent)
            })
            .sum::<u32>();

        println!("{total_score}");
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RockPaperScissors {
    Rock,
    Paper,
    Scissors,
}

impl RockPaperScissors {
    const ORDER: [Self; 3] = [Self::Rock, Self::Paper, Self::Scissors];

    fn from_byte(s: u8) -> Self {
        match s {
            b'A' | b'X' => Self::Rock,
            b'B' | b'Y' => Self::Paper,
            b'C' | b'Z' => Self::Scissors,
            _ => panic!("unsupported byte"),
        }
    }

    fn score_against(self, other: Self) -> u32 {
        let base_score = match () {
            _ if self == other => 3,
            _ if self == Self::winning_against(other) => 6,
            _ => 0,
        };

        base_score + (self as u32 + 1)
    }

    fn winning_against(other: Self) -> Self {
        Self::ORDER[(other as usize + 1) % 3]
    }

    fn losing_against(other: Self) -> Self {
        Self::ORDER[(other as usize + 2) % 3]
    }
}
