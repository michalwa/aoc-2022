use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub fn solve(input: BufReader<File>) {
    let total_score = input
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let line = line.trim().as_bytes();
            let opponent = rock_paper_scissors(line[0]);

            // Part 1
            // let player = rock_paper_scissors(line[2]);
            // let score = match () {
            //     _ if opponent == prev(player) => 6,
            //     _ if opponent == player => 3,
            //     _ => 0,
            // };
            //
            // score + player as u32 + 1

            // Part 2
            (match line[2] {
                b'X' => prev(opponent) + 1,
                b'Y' => opponent + 1 + 3,
                b'Z' => next(opponent) + 1 + 6,
                _ => unreachable!(),
            }) as u32
        })
        .sum::<u32>();

    println!("{total_score}");
}

fn next(rps: u8) -> u8 { (rps + 1) % 3 }
fn prev(rps: u8) -> u8 { (rps + 2) % 3 }

fn rock_paper_scissors(byte: u8) -> u8 {
    match byte {
        b'A' | b'X' => 0,
        b'B' | b'Y' => 1,
        b'C' | b'Z' => 2,
        _ => unreachable!(),
    }
}
