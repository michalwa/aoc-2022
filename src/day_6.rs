use crate::Day;
use std::{
    fs::File,
    io::{BufReader, Read},
};

pub struct Day6;

impl Day for Day6 {
    fn solve(&self, mut input: BufReader<File>) {
        let num_distinct = 14; /* Part 1: 4 */

        let mut string = String::new();
        input.read_to_string(&mut string).unwrap();

        let string = string.trim();

        let position = string
            .as_bytes()
            .windows(num_distinct)
            .position(|window| {
                window
                    .iter()
                    .map(|byte| 1u32 << (byte - b'a'))
                    .fold(0, |a, b| a | b)
                    .count_ones()
                    == num_distinct as u32
            })
            .unwrap();

        println!("{}", position + num_distinct);
    }
}
