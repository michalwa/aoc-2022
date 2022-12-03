#![feature(iter_array_chunks)]

use std::{collections::HashMap, env, fs::File, io::BufReader};

mod day_1;
mod day_2;
mod day_3;

use day_1::Day1;
use day_2::Day2;
use day_3::Day3;

const USAGE: &str = "Usage: aoc-2022 <day> <input-file>";

trait Day {
    fn solve(&self, input: BufReader<File>);
}

fn main() {
    let days = {
        let mut days = HashMap::<_, Box<dyn Day>>::new();
        days.insert("day_1", Box::new(Day1));
        days.insert("day_2", Box::new(Day2));
        days.insert("day_3", Box::new(Day3));
        days
    };

    let mut args = env::args().into_iter().skip(1);
    let day_name = args.next().expect(USAGE);
    let input_filename = args.next().expect(USAGE);

    let file = File::open(input_filename).unwrap();
    days.get(&*day_name)
        .expect("no such day")
        .solve(BufReader::new(file));
}
