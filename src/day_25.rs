use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub fn solve(input: BufReader<File>) {
    let result = input
        .lines()
        .map(Result::unwrap)
        .map(|l| parse_snafu(l.trim()))
        .sum::<i64>();

    println!("{}", to_snafu(result));
}

const DIGITS: &str = "=-012";

fn parse_snafu(s: &str) -> i64 {
    s.chars()
        .map(|d| DIGITS.find(d).unwrap() as i64 - 2)
        .fold(0, |a, d| a * 5 + d)
}

fn to_snafu(mut n: i64) -> String {
    let mut result = Vec::new();

    while n > 0 {
        n += 2;
        let digit = DIGITS.as_bytes()[(n % 5) as usize];
        result.push(digit);
        n /= 5;
    }

    result.reverse();
    String::from_utf8(result).unwrap()
}
