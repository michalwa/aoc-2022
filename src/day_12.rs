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

    // Part 1
    // let path = astar(&rows, start.unwrap(), end);
    // println!("{}", path.len() - 1);

    // Part 2
    let mut min_len = usize::MAX;

    for (i, row) in rows.iter().enumerate() {
        for (j, c) in row.iter().enumerate() {
            if elevation(*c) == b'a' {
                let mut astar = Astar::new(&rows, [i, j], end);

                if let Some(path) = astar.find_path() {
                    min_len = min_len.min(path.len());
                }
            }
        }
    }

    println!("{}", min_len - 1);
}

struct Astar<'a> {
    rows: &'a [Vec<u8>],
    start: [usize; 2],
    end: [usize; 2],
    open_set: HashSet<[usize; 2]>,
    came_from: HashMap<[usize; 2], [usize; 2]>,
    g_score: HashMap<[usize; 2], u32>,
    f_score: HashMap<[usize; 2], u32>,
}

impl<'a> Astar<'a> {
    fn new(rows: &'a [Vec<u8>], start: [usize; 2], end: [usize; 2]) -> Self {
        Self {
            rows,
            start,
            end,
            open_set: HashSet::from([start]),
            came_from: HashMap::new(),
            g_score: HashMap::new(),
            f_score: HashMap::new(),
        }
    }

    fn find_path(&mut self) -> Option<Vec<[usize; 2]>> {
        self.g_score.clear();
        self.g_score.insert(self.start, 0);

        self.f_score.clear();
        self.f_score.insert(self.start, self.heuristic(self.start));

        while !self.open_set.is_empty() {
            let current = self
                .open_set
                .iter()
                .copied()
                .min_by_key(|n| self.f_score.get(n).unwrap_or(&u32::MAX))
                .unwrap();

            if current == self.end {
                return Some(self.reconstruct_path(current));
            }

            self.open_set.remove(&current);

            for neighbor in self.neighbors(current).collect::<Vec<_>>() {
                let new_g_score = self.g_score[&current] + 1;

                if !self.g_score.contains_key(&neighbor)
                    || new_g_score < self.g_score[&neighbor]
                {
                    self.came_from.insert(neighbor, current);
                    self.g_score.insert(neighbor, new_g_score);
                    self.f_score.insert(neighbor, new_g_score + self.heuristic(neighbor));
                    self.open_set.insert(neighbor);
                }
            }
        }

        None
    }

    fn heuristic(&self, cell: [usize; 2]) -> u32 {
        (cell[0].abs_diff(self.end[0]) + cell[1].abs_diff(self.end[1])) as u32
    }

    fn neighbors(
        &self,
        cell: [usize; 2],
    ) -> impl Iterator<Item = [usize; 2]> + '_ {
        let cell_elevation = elevation(self.rows[cell[0]][cell[1]]);

        [[-1, 0], [1, 0], [0, -1], [0, 1]]
            .into_iter()
            .filter_map(move |offset| self.offset(cell, offset))
            .filter(move |&[i, j]| {
                elevation(self.rows[i][j]) <= cell_elevation + 1
            })
    }

    fn offset(
        &self,
        cell: [usize; 2],
        offset: [isize; 2],
    ) -> Option<[usize; 2]> {
        let new_cell =
            [cell[0] as isize + offset[0], cell[1] as isize + offset[1]];

        if (0..self.rows.len() as isize).contains(&new_cell[0])
            && (0..self.rows[0].len() as isize).contains(&new_cell[1])
        {
            Some([new_cell[0] as usize, new_cell[1] as usize])
        } else {
            None
        }
    }

    fn reconstruct_path(&mut self, mut current: [usize; 2]) -> Vec<[usize; 2]> {
        let mut path = vec![current];

        while self.came_from.contains_key(&current) {
            current = self.came_from[&current];
            path.insert(0, current);
        }

        path
    }
}

fn elevation(byte: u8) -> u8 {
    match byte {
        b'S' => b'a',
        b'E' => b'z',
        b => b,
    }
}
