use std::{
    collections::{HashSet, VecDeque},
    fs,
    ops::{Mul, Sub},
};

type Number = i16;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct Matrix([[Number; 4]; 4]);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct Point([Number; 4]);

struct Variation {
    rotation: Matrix,
    beacons: Vec<Point>,
}

struct Scanner {
    position: Option<Matrix>,
    beacons: Vec<Point>,
    variations: Vec<Variation>,
}

type Beacons = Vec<Point>;

impl Sub<&Point> for &Point {
    type Output = Point;

    fn sub(self, rhs: &Point) -> Self::Output {
        Point([
            self.0[0] - rhs.0[0],
            self.0[1] - rhs.0[1],
            self.0[2] - rhs.0[2],
            1,
        ])
    }
}

impl Mul<&Matrix> for &Matrix {
    type Output = Matrix;

    fn mul(self, rhs: &Matrix) -> Self::Output {
        let mut result = Matrix([[0; 4]; 4]);

        for i in 0..4 {
            for j in 0..4 {
                for k in 0..4 {
                    result.0[i][j] += self.0[i][k] * rhs.0[k][j];
                }
            }
        }

        result
    }
}

impl Mul<&Point> for &Matrix {
    type Output = Point;

    fn mul(self, rhs: &Point) -> Self::Output {
        Point([
            self.0[0][0] * rhs.0[0]
                + self.0[0][1] * rhs.0[1]
                + self.0[0][2] * rhs.0[2]
                + self.0[0][3],
            self.0[1][0] * rhs.0[0]
                + self.0[1][1] * rhs.0[1]
                + self.0[1][2] * rhs.0[2]
                + self.0[1][3],
            self.0[2][0] * rhs.0[0]
                + self.0[2][1] * rhs.0[1]
                + self.0[2][2] * rhs.0[2]
                + self.0[2][3],
            1,
        ])
    }
}

static ROTATE_X: Matrix = Matrix([[1, 0, 0, 0], [0, 0, -1, 0], [0, 1, 0, 0], [0, 0, 0, 1]]);
static ROTATE_Y: Matrix = Matrix([[0, 0, 1, 0], [0, 1, 0, 0], [-1, 0, 0, 0], [0, 0, 0, 1]]);
static ROTATE_Z: Matrix = Matrix([[0, -1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]);
static IDENTITY: Matrix = Matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]);
static ORIGIN: Point = Point([0, 0, 0, 1]);

impl Matrix {
    fn translate(point: Point) -> Matrix {
        Matrix([
            [1, 0, 0, point.0[0]],
            [0, 1, 0, point.0[1]],
            [0, 0, 1, point.0[2]],
            [0, 0, 0, 1],
        ])
    }

    fn rotations() -> Vec<Matrix> {
        let mut result = vec![];
        let mut matrix = IDENTITY;

        for i in 0..6 {
            if i % 2 == 0 {
                matrix = &matrix * &ROTATE_X;
            } else {
                matrix = &matrix * &ROTATE_Y;
            }

            for _ in 0..4 {
                matrix = &matrix * &ROTATE_Z;
                result.push(matrix.clone());
            }
        }

        result
    }
}

impl Point {
    fn new(x: Number, y: Number, z: Number) -> Point {
        Point([x, y, z, 1])
    }

    fn manhattan(&self, other: &Point) -> Number {
        (self - other).0.iter().map(|n| n.abs()).sum::<Number>() - 1
    }
}

fn parse_input() -> Vec<Beacons> {
    fs::read_to_string("../inputs/19")
        .unwrap()
        .split("\n\n")
        .map(|scanner| {
            scanner
                .lines()
                .into_iter()
                .skip(1)
                .map(|line| {
                    let parts: Vec<Number> = line.split(",").map(|p| p.parse().unwrap()).collect();
                    Point::new(parts[0], parts[1], parts[2])
                })
                .collect()
        })
        .collect()
}

fn overlap_transformation(first: &Scanner, second: &Scanner) -> Option<Matrix> {
    for origin in first.beacons.iter().skip(11) {
        for variation in second.variations.iter() {
            for other in variation.beacons.iter() {
                let translation = Matrix::translate(origin - other);
                let overlapping = variation
                    .beacons
                    .iter()
                    .map(|p| &translation * p)
                    .filter(|x| first.beacons.contains(x))
                    .count();

                if overlapping >= 12 {
                    return Some(&first.position.unwrap() * &(&translation * &variation.rotation));
                }
            }
        }
    }

    None
}

fn prepare_scanners() -> Vec<Scanner> {
    let mut scanners: Vec<Scanner> = parse_input()
        .into_iter()
        .map(|beacons| Scanner {
            position: None,
            beacons,
            variations: vec![],
        })
        .collect();

    let rotations = Matrix::rotations();

    for scanner in scanners.iter_mut() {
        for rotation in rotations.iter() {
            let beacons = scanner
                .beacons
                .iter()
                .map(|beacon| rotation * beacon)
                .collect();

            scanner.variations.push(Variation {
                beacons,
                rotation: rotation.clone(),
            });
        }
    }

    scanners
}

fn merge(scanners: &mut Vec<Scanner>) {
    scanners[0].position = Some(IDENTITY.clone());

    let mut known: VecDeque<usize> = VecDeque::new();
    known.push_back(0);

    while let Some(i) = known.pop_front() {
        let scanner = &scanners[i];
        let mut found: Vec<(usize, Matrix)> = vec![];

        for (i, unknown) in scanners.iter().enumerate().filter(|s| s.1.position == None) {
            if let Some(matrix) = overlap_transformation(scanner, unknown) {
                found.push((i, matrix));
            }
        }

        for (i, position) in found {
            known.push_back(i);
            scanners[i].position = Some(position)
        }
    }

    assert!(scanners.iter().all(|s| s.position != None))
}

fn main() {
    let mut scanners = prepare_scanners();

    merge(&mut scanners);

    let beacons: HashSet<Point> = scanners
        .iter()
        .flat_map(|scanner| {
            scanner
                .beacons
                .iter()
                .map(|beacon| &scanner.position.unwrap() * beacon)
        })
        .collect();
    let beacon_count = beacons.len();

    let scanner_positions: Vec<Point> = scanners
        .iter()
        .map(|s| &s.position.unwrap() * &ORIGIN)
        .collect();

    let mut max_distance = 0;
    for x in scanner_positions.iter() {
        for y in scanner_positions.iter() {
            max_distance = max_distance.max(x.manhattan(y));
        }
    }

    println!("first  = {}", beacon_count);
    println!("second = {}", max_distance);
}
