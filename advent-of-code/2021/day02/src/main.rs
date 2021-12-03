use std::fs;

enum Direction {
    Forward(i64),
    Up(i64),
    Down(i64),
}

use Direction::*;

fn main() {
    println!("first = {:?}", first());
    println!("second = {:?}", second());
}

fn directions() -> Vec<Direction> {
    fs::read_to_string("../inputs/02")
        .unwrap()
        .lines()
        .map(|line| {
            let words = line.split(" ").collect::<Vec<_>>();
            let amount = words[1].parse::<i64>().unwrap();
            match words[0] {
                "forward" => Forward(amount),
                "up" => Up(amount),
                "down" => Down(amount),
                _ => panic!(),
            }
        })
        .collect()
}

fn first() -> i64 {
    let mut x = 0;
    let mut y = 0;

    for direction in directions() {
        match direction {
            Forward(delta) => x += delta,
            Up(delta) => y -= delta,
            Down(delta) => y += delta,
        }
    }

    x * y
}

fn second() -> i64 {
    let mut x = 0;
    let mut y = 0;
    let mut aim = 0;

    for direction in directions() {
        match direction {
            Forward(delta) => {
                x += delta;
                y += aim * delta
            }
            Up(delta) => aim -= delta,
            Down(delta) => aim += delta,
        }
    }

    x * y
}
