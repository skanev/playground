use std::{collections::HashSet, fs};

type Number = i32;
type Point = (Number, Number);

fn parse_input() -> (String, HashSet<Point>) {
    let input = fs::read_to_string("../inputs/20").unwrap();
    let mut data = input.split("\n\n");
    let decoder = data.next().unwrap().to_string();
    let points: HashSet<Point> = data
        .next()
        .unwrap()
        .lines()
        .enumerate()
        .flat_map(|(x, line)| {
            line.bytes()
                .enumerate()
                .filter(|(_, c)| *c == b'#')
                .map(move |(y, _)| (x as Number, y as Number))
        })
        .collect();

    (decoder, points)
}

fn iterate(points: &HashSet<Point>, decoder: &String, mode: bool) -> HashSet<Point> {
    let mut result = HashSet::new();

    let deltas = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 0),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];

    let min_x = points.iter().map(|p| p.0).min().unwrap_or(0);
    let max_x = points.iter().map(|p| p.0).max().unwrap_or(0);
    let min_y = points.iter().map(|p| p.1).min().unwrap_or(0);
    let max_y = points.iter().map(|p| p.1).max().unwrap_or(0);

    for x in (min_x - 5)..=(max_x + 5) {
        for y in (min_y - 5)..=(max_y + 5) {
            let index = deltas
                .iter()
                .map(|(dx, dy)| {
                    if points.contains(&(x + dx, y + dy)) == mode {
                        1usize
                    } else {
                        0usize
                    }
                })
                .fold(0, |a, b| (a << 1) + b);

            if (decoder.as_bytes()[index] == b'#') != mode {
                result.insert((x, y));
            }
        }
    }

    result
}

fn main() {
    let (decoder, points) = parse_input();
    let mut points = points;

    points = iterate(&points, &decoder, true);
    points = iterate(&points, &decoder, false);

    let first = points.len();

    for i in 0..48 {
      points = iterate(&points, &decoder, i % 2 == 0);
    }

    let second = points.len();

    println!("first  = {}", first);
    println!("second = {}", second);
}
