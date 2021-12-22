use std::fs;

type Num = i64;

#[derive(Copy, Clone)]
struct Interval {
    from: Num,
    to: Num,
}

impl Interval {
    fn new(from: Num, to: Num) -> Interval {
        Interval {
            from,
            to: from.max(to),
        }
    }

    fn truncate(&self, other: &Interval) -> Interval {
        Interval::new(self.from.max(other.from), self.to.min(other.to))
    }

    fn overlaps(&self, other: &Interval) -> bool {
        self.from < other.to && other.from < self.to
    }

    fn len(&self) -> Num {
        self.to - self.from
    }

    fn split(a: &Interval, b: &Interval) -> [Interval; 3] {
        let mut points = vec![a.from, a.to, b.from, b.to];
        points.sort();
        [
            Interval::new(points[0], points[1]),
            Interval::new(points[1], points[2]),
            Interval::new(points[2], points[3]),
        ]
    }
}

#[derive(Copy, Clone)]
struct Cuboid {
    x: Interval,
    y: Interval,
    z: Interval,
}

impl Cuboid {
    fn truncate(&self, interval: Interval) -> Cuboid {
        Cuboid {
            x: self.x.truncate(&interval),
            y: self.y.truncate(&interval),
            z: self.z.truncate(&interval),
        }
    }

    fn overlaps(&self, other: &Cuboid) -> bool {
        self.x.overlaps(&other.x) && self.y.overlaps(&other.y) && self.z.overlaps(&other.z)
    }

    fn subtract(&self, other: &Cuboid) -> Vec<Cuboid> {
        if !self.overlaps(other) {
            return vec![self.clone()];
        }

        let mut result = vec![];

        for x in Interval::split(&self.x, &other.x) {
            for y in Interval::split(&self.y, &other.y) {
                for z in Interval::split(&self.z, &other.z) {
                    let cuboid = Cuboid { x, y, z };

                    if self.overlaps(&cuboid) && !other.overlaps(&cuboid) {
                        result.push(cuboid)
                    }
                }
            }
        }

        result
    }

    fn volume(&self) -> Num {
        self.x.len() * self.y.len() * self.z.len()
    }
}

fn parse_input() -> Vec<(Cuboid, bool)> {
    fs::read_to_string("../inputs/22")
        .unwrap()
        .lines()
        .map(|line| {
            let chunks: Vec<&str> = line.split(" ").collect();
            let on = chunks[0] == "on";

            let intervals: Vec<Interval> = chunks[1]
                .split(",")
                .map(|part| part.split("=").nth(1).unwrap())
                .map(|range| {
                    let numbers: Vec<Num> = range.split("..").map(|n| n.parse().unwrap()).collect();
                    Interval {
                        from: numbers[0],
                        to: numbers[1] + 1,
                    }
                })
                .collect();

            let cuboid = Cuboid {
                x: intervals[0],
                y: intervals[1],
                z: intervals[2],
            };

            (cuboid, on)
        })
        .collect()
}

fn solve(input: Vec<(Cuboid, bool)>) -> Num {
    let mut cuboids: Vec<Cuboid> = vec![];

    for (cuboid, on) in input {
        cuboids = cuboids
            .into_iter()
            .flat_map(|c| c.subtract(&cuboid))
            .collect();

        if on {
            cuboids.push(cuboid)
        }
    }

    cuboids.into_iter().map(|c| c.volume()).sum::<Num>()
}

fn main() {
    let input = parse_input();

    let first_input = input
        .iter()
        .map(|(cuboid, on)| (cuboid.truncate(Interval::new(-50, 50)), *on))
        .collect::<Vec<_>>();

    println!("first  = {}", solve(first_input));
    println!("second = {}", solve(input));
}
