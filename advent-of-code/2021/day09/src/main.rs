use std::{collections::VecDeque, fs};

fn parse_input() -> Vec<Vec<u8>> {
    let text = fs::read_to_string("../inputs/09").unwrap();
    let height = text.lines().count();
    let width = text.lines().next().unwrap().len();

    let mut result = vec![vec![9; width + 2]; height + 2];

    for (i, line) in text.lines().enumerate() {
        for (j, byte) in line.bytes().enumerate() {
            result[i + 1][j + 1] = byte - b'0';
        }
    }

    result
}

fn low_points(map: &Vec<Vec<u8>>) -> Vec<(usize, usize)> {
    let height = map.len();
    let width = map[0].len();

    let mut result = vec![];

    for i in 1..(height - 2) {
        for j in 1..(width - 2) {
            if map[i][j] < map[i - 1][j]
                && map[i][j] < map[i + 1][j]
                && map[i][j] < map[i][j - 1]
                && map[i][j] < map[i][j + 1]
            {
                result.push((i, j));
            }
        }
    }

    result
}

fn fill(map: &mut Vec<Vec<u8>>, point: (usize, usize)) -> usize {
    let mut left: VecDeque<(usize, usize)> = VecDeque::new();
    let mut count = 0;

    left.push_back(point);

    while left.len() > 0 {
        let point = left.pop_front().unwrap();

        if map[point.0][point.1] == 9 {
            continue;
        }

        map[point.0][point.1] = 9;
        count += 1;

        left.push_back((point.0 - 1, point.1));
        left.push_back((point.0 + 1, point.1));
        left.push_back((point.0, point.1 - 1));
        left.push_back((point.0, point.1 + 1));
    }

    count
}

fn main() {
    let mut map = parse_input();
    let lows = low_points(&map);

    let first: usize = lows.iter().map(|&(x, y)| (map[x][y] as usize) + 1).sum();

    let mut basins: Vec<usize> = vec![];

    for point in lows {
        basins.push(fill(&mut map, point));
    }
    basins.sort();

    let second: usize = basins[basins.len() - 3..].iter().product();

    println!("first  = {}", first);
    println!("second = {}", second);
}
