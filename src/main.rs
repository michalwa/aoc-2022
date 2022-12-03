use std::{collections::HashMap, env, fs::File, io::BufReader};

mod day_1;

use day_1::Day1;

const USAGE: &str = "Usage: aoc-2022 <day> <input-file>";

trait Day {
    fn solve(&self, input: BufReader<File>);
}

fn main() {
    let days = {
        let mut days = HashMap::<_, Box<dyn Day>>::new();
        days.insert("day_1", Box::new(Day1));
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
