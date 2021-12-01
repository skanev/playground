use std::fs;

fn main() {
    println!("first = {:?}", first());
    println!("second = {:?}", second());
}

fn numbers() -> Vec<i64> {
    fs::read_to_string("../inputs/01")
        .unwrap()
        .lines()
        .map(|line| line.parse::<i64>().unwrap())
        .collect::<Vec<i64>>()
}

fn first() -> usize {
    numbers().windows(2).filter(|a| a[1] > a[0]).count()
}

fn second() -> usize {
    numbers()
        .windows(3)
        .map(|a| a[0] + a[1] + a[2])
        .collect::<Vec<_>>()
        .windows(2)
        .filter(|a| a[1] > a[0])
        .count()
}
