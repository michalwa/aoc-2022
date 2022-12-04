#![feature(iter_array_chunks)]

use std::{env, fs::File, io::BufReader};

include!(concat!(env!("OUT_DIR"), "/days.rs"));

const USAGE: &str = "Usage: aoc-2022 <day> <input-file>";

trait Day {
    fn solve(&self, input: BufReader<File>);
}

fn main() {
    let mut args = env::args().into_iter().skip(1);
    let day_name = args.next().expect(USAGE);
    let input_filename = args.next().expect(USAGE);

    let file = File::open(input_filename).unwrap();
    days()
        .get(&*day_name)
        .expect("no such day")
        .solve(BufReader::new(file));
}
