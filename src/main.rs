#![feature(iter_array_chunks)]

use std::{
    env,
    fs::File,
    io::{BufReader, Write},
    process::{Command, Stdio},
};

include!(concat!(env!("OUT_DIR"), "/days.rs"));

const USAGE: &str = "Usage: aoc-2022 <solution-file> <input-name>";

fn main() {
    let mut args = env::args().into_iter().skip(1);
    let solution_file_name = args.next().expect(USAGE);
    let input_name = args.next().expect(USAGE);

    let (solution_name, _) = solution_file_name
        .rsplit_once('.')
        .expect("solution-file must have an extension");

    let input_path = format!("input/{solution_name}/{input_name}.txt");
    solve(&solution_file_name, &input_path);
}

fn run_rust(solve: impl FnOnce(BufReader<File>), input_path: &str) {
    let file = File::open(input_path).unwrap();
    solve(BufReader::new(file));
}

fn run_prolog(path: &str, input_path: &str) {
    let program =
        env::var("PROLOG").unwrap_or_else(|_| "scryer-prolog".to_owned());

    let mut process = Command::new(program)
        .args([path, "-g", "run"])
        .stdin(Stdio::piped())
        .spawn()
        .expect("failed to spawn prolog interpreter");

    write!(process.stdin.as_mut().unwrap(), "'{input_path}'.").unwrap();

    process.wait().ok();
}
