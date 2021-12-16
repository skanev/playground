use std::{
    cmp::Reverse,
    collections::BinaryHeap,
    fs,
};

fn parse_input() -> Vec<Vec<i64>> {
    fs::read_to_string("../inputs/15")
        .unwrap()
        .lines()
        .map(|line| {
            line.as_bytes()
                .iter()
                .map(|b| (b - b'0') as i64)
                .collect()
        })
        .collect()
}

fn adjancent(x: usize, y: usize, h: usize, w: usize) -> Vec<(usize, usize)> {
    let mut result = vec![];

    if x > 0 {
        result.push((x - 1, y))
    }
    if x < h - 1 {
        result.push((x + 1, y))
    }
    if y > 0 {
        result.push((x, y - 1))
    }
    if y < w - 1 {
        result.push((x, y + 1))
    }

    result
}

fn five_expand(grid: &Vec<Vec<i64>>) -> Vec<Vec<i64>> {
    let h = grid.len();
    let w = grid[0].len();
    let mut result = vec![vec![i64::MAX; w * 5]; h * 5];

    for i in 0..(h * 5) {
        for j in 0..(w * 5) {
            let increment = i / h + j / w;
            let extra = grid[i % w][j % w] + increment as i64;
            result[i][j] = extra % 10 + extra / 10;
        }
    }

    result
}

fn lowest_risk(grid: &Vec<Vec<i64>>) -> i64 {
    let h = grid.len();
    let w = grid[0].len();

    let mut total: Vec<Vec<i64>> = vec![vec![i64::MAX; w]; h];
    let mut left: BinaryHeap<Reverse<(i64, usize, usize)>> = BinaryHeap::new();

    left.push(Reverse((grid[h - 1][w - 1], h - 1, w - 1)));
    total[h - 1][w - 1] = grid[h - 1][w - 1];

    while let Some(Reverse((d, x, y))) = left.pop() {
        for (i, j) in adjancent(x, y, h, w) {
            if i == 0 && j == 0 {
                return d
            }
            let risk = total[x][y] + grid[i][j];
            if risk < total[i][j] {
                total[i][j] = risk;
                left.push(Reverse((risk, i, j)));
            }
        }
    }

    i64::MAX
}

fn main() {
    let first = parse_input();
    let second = five_expand(&first);

    println!("first  = {}", lowest_risk(&first));
    println!("second = {}", lowest_risk(&second));
}
