#![feature(iter_array_chunks)]

use std::{env, fs::File, io::BufReader};

include!(concat!(env!("OUT_DIR"), "/days.rs"));

const USAGE: &str = "Usage: aoc-2022 <day> <input-filename>";

fn main() {
    let mut args = env::args().into_iter().skip(1);
    let day_name = args.next().expect(USAGE);
    let input_filename = args.next().expect(USAGE);
    let input_path = format!("input/{day_name}/{input_filename}.txt");

    let file = File::open(input_path).unwrap();
    solve(&day_name, BufReader::new(file));
}
