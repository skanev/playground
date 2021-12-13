use std::{collections::HashSet, fs};

type Point = (usize, usize);
type Fold = (char, usize);

fn parse_input() -> (HashSet<Point>, Vec<Fold>) {
    let text = fs::read_to_string("../inputs/13").unwrap();
    let mut input = text.split("\n\n");

    let points = input
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            let mut parts = line.split(",");
            let x = parts.next().unwrap().parse().unwrap();
            let y = parts.next().unwrap().parse().unwrap();
            (x, y)
        })
        .collect();

    let folds = input
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            let mut parts = line.split("=");
            let axis = parts.next().unwrap().chars().last().unwrap();
            let coordinate = parts.next().unwrap().parse().unwrap();

            (axis, coordinate)
        })
        .collect();

    (points, folds)
}

fn fold(points: HashSet<Point>, fold: Fold) -> HashSet<Point> {
    let (axis, point) = fold;

    points
        .iter()
        .map(|&(x, y)| {
            (
                if axis == 'x' && x > point { 2 * point - x } else { x },
                if axis == 'y' && y > point { 2 * point - y } else { y },
            )
        })
        .collect()
}

fn draw(points: &HashSet<Point>) {
    let h = points.iter().map(|(_, b)| *b).max().unwrap();
    let w = points.iter().map(|(a, _)| *a).max().unwrap();

    for x in 0..=h {
        for y in 0..=w {
            if points.contains(&(y, x)) {
                print!("#")
            } else {
                print!(" ")
            }
        }
        println!("")
    }
}

fn main() {
    let (points, folds) = parse_input();

    let first = fold(points.clone(), folds[0]).len();
    println!("first = {}", first);

    let folded = folds.iter().fold(points, |p, &f| fold(p, f));
    draw(&folded);
}
