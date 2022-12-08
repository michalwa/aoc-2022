use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub struct Day8;

impl Day for Day8 {
    fn solve(&self, input: BufReader<File>) {
        let mut rows = input
            .lines()
            .map(Result::unwrap)
            .map(|line| {
                line.chars()
                    .map(|c| (false, c.to_digit(10).unwrap() as i32))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        for row in &mut rows {
            do_scan(row.iter_mut());
            do_scan(row.iter_mut().rev());
        }

        for i in 0..rows[0].len() {
            do_scan(rows.iter_mut().map(|row| &mut row[i]));
            do_scan(rows.iter_mut().map(|row| &mut row[i]).rev());
        }

        let visible_count = rows
            .into_iter()
            .map(|row| row.into_iter().filter(|(visible, _)| *visible).count())
            .sum::<usize>();

        println!("{visible_count}");
    }
}

fn do_scan<'a>(row: impl IntoIterator<Item = &'a mut (bool, i32)>) {
    let mut max_so_far = -1;

    for (visible, tree) in row {
        if *tree > max_so_far {
            *visible = true;
            max_so_far = *tree;
        }
    }
}
