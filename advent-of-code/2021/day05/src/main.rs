use std::collections::HashMap;
use std::fs;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}

#[derive(Debug, Hash)]
struct Line {
    from: Point,
    to: Point,
}

fn parse_input() -> Vec<Line> {
    fs::read_to_string("../inputs/05")
        .unwrap()
        .lines()
        .map(|line| {
            let parts: Vec<Vec<i64>> = line
                .split(" -> ")
                .map(|part| part.split(",").map(|d| d.parse::<i64>().unwrap()).collect())
                .collect();

            Line {
                from: Point {
                    x: parts[0][0],
                    y: parts[0][1],
                },
                to: Point {
                    x: parts[1][0],
                    y: parts[1][1],
                },
            }
        })
        .collect()
}

fn range(a: i64, b: i64) -> Box<dyn Iterator<Item = i64>> {
    if a > b {
        Box::new((b..=a).rev())
    } else {
        Box::new((a..=b).into_iter())
    }
}

impl Line {
    fn is_diagonal(&self) -> bool {
        self.from.x != self.to.x && self.from.y != self.to.y
    }

    fn points(&self) -> Vec<Point> {
        if self.from.x == self.to.x {
            range(self.from.y, self.to.y)
                .map(|y| Point { x: self.from.x, y })
                .collect()
        } else if self.from.y == self.to.y {
            range(self.from.x, self.to.x)
                .map(|x| Point { x, y: self.from.y })
                .collect()
        } else {
            range(self.from.x, self.to.x)
                .zip(range(self.from.y, self.to.y))
                .map(|(x, y)| Point { x, y })
                .collect()
        }
    }
}

fn intersections<'a>(lines: impl Iterator<Item = &'a Line>) -> usize {
    let mut counts: HashMap<Point, usize> = HashMap::new();

    for point in lines.flat_map(|line| line.points()) {
        let count = counts.entry(point).or_insert(0);
        *count += 1;
    }

    counts.values().filter(|c| **c > 1).count()
}

fn main() {
    let lines = parse_input();

    println!(
        "first  = {}",
        intersections(lines.iter().filter(|line| !line.is_diagonal()))
    );
    println!("second = {}", intersections(lines.iter()));
}
