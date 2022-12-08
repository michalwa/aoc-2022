use crate::Day;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub struct Day8;

impl Day for Day8 {
    fn solve(&self, input: BufReader<File>) {
        // self.part_1(input);
        self.part_2(input);
    }
}

#[allow(dead_code)]
impl Day8 {
    fn part_1(&self, input: BufReader<File>) {
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

    fn part_2(&self, input: BufReader<File>) {
        let rows = input
            .lines()
            .map(Result::unwrap)
            .map(|line| {
                line.chars()
                    .map(|c| c.to_digit(10).unwrap())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();


        let mut max_score = 0;

        for (i, row) in rows.iter().enumerate().skip(1).take(rows.len() - 2) {
            for (j, &tree) in row.iter().enumerate().skip(1).take(row.len() - 2)
            {
                let score = [
                    count_visible_trees(tree, (0..i).rev().map(|i| rows[i][j])),
                    count_visible_trees(
                        tree,
                        ((i + 1)..rows.len()).map(|i| rows[i][j]),
                    ),
                    count_visible_trees(tree, row[..j].iter().rev().copied()),
                    count_visible_trees(tree, row[(j + 1)..].iter().copied()),
                ]
                .into_iter()
                .product::<usize>();

                max_score = max_score.max(score);
            }
        }

        println!("{max_score}");
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

fn count_visible_trees(
    from_tree: u32,
    trees: impl Iterator<Item = u32> + Clone,
) -> usize {
    let tree_lower = |other_tree: &u32| *other_tree < from_tree;

    trees
        .clone()
        .take_while(tree_lower)
        .chain(trees.filter(|t| !tree_lower(t)).take(1))
        .count()
}
