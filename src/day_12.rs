use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
};

pub fn solve(input: BufReader<File>) {
    let rows = input
        .lines()
        .map(Result::unwrap)
        .map(String::into_bytes)
        .collect::<Vec<_>>();

    let mut start = None;
    let mut end = None;

    for (i, row) in rows.iter().enumerate() {
        for (j, c) in row.iter().enumerate() {
            match c {
                b'S' => start = Some([i, j]),
                b'E' => end = Some([i, j]),
                _ => (),
            }
        }
    }

    let end = end.unwrap();
    let path = astar(&rows, start.unwrap(), end);

    // Part 1
    // println!("{}", path.len() - 1);

    let mut min_len = usize::MAX;

    for (i, row) in rows.iter().enumerate() {
        for (j, c) in row.iter().enumerate() {
            if elevation(*c) == b'a' {
                if let Some(path) = astar(&rows, [i, j], end) {
                    min_len = min_len.min(path.len());
                }
            }
        }
    }

    println!("{}", min_len - 1);
}

// bardzo przepraszam za ten kod

fn astar(
    rows: &[Vec<u8>],
    start: [usize; 2],
    end: [usize; 2],
) -> Option<Vec<[usize; 2]>> {
    let h =
        |n: [usize; 2]| (end[0].abs_diff(n[0]) + end[1].abs_diff(n[1])) as u32;

    let neighbors = |[i, j]: [usize; 2]| {
        let mut ns = Vec::new();

        for [di, dj] in [[-1, 0], [1, 0], [0, -1], [0, 1]] {
            let ni = i as isize + di;
            let nj = j as isize + dj;

            if !(0..rows.len() as isize).contains(&ni)
                || !(0..rows[0].len() as isize).contains(&nj)
            {
                continue;
            }

            let e = elevation(rows[i][j]);
            let ne = elevation(rows[ni as usize][nj as usize]);

            if ne <= e + 1 {
                ns.push([ni as usize, nj as usize]);
            }
        }

        ns
    };

    fn reconstruct_path(
        came_from: HashMap<[usize; 2], [usize; 2]>,
        mut current: [usize; 2],
    ) -> Vec<[usize; 2]> {
        let mut path = vec![current];

        while came_from.contains_key(&current) {
            current = came_from[&current];
            path.insert(0, current);
        }

        path
    }

    let mut open_set = HashSet::from([start]);
    let mut came_from: HashMap<[usize; 2], [usize; 2]> = HashMap::new();

    let mut g_score: HashMap<[usize; 2], u32> = HashMap::new();
    g_score.insert(start, 0);

    let mut f_score: HashMap<[usize; 2], u32> = HashMap::new();
    f_score.insert(start, h(start));

    while !open_set.is_empty() {
        let current = open_set
            .iter()
            .copied()
            .min_by_key(|n| f_score.get(n).unwrap_or(&u32::MAX))
            .unwrap();

        if current == end {
            return Some(reconstruct_path(came_from, current));
        }

        open_set.remove(&current);

        for n in neighbors(current) {
            let tent_g_score = g_score[&current] + 1;

            if !g_score.contains_key(&n) || tent_g_score < g_score[&n] {
                came_from.insert(n, current);
                g_score.insert(n, tent_g_score);
                f_score.insert(n, tent_g_score + h(n));

                open_set.insert(n);
            }
        }
    }

    None
}

fn elevation(byte: u8) -> u8 {
    match byte {
        b'S' => b'a',
        b'E' => b'z',
        b => b,
    }
}
