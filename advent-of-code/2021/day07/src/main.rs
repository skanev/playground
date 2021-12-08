use std::fs::read_to_string;

fn parse_input() -> Vec<i64> {
    let input = read_to_string("../inputs/07").unwrap();
    input[0..input.len() - 1]
        .split(",")
        .map(|chunk| chunk.parse().unwrap())
        .collect()
}

fn main() {
    let numbers = parse_input();
    let max = *numbers.iter().max().unwrap();
    let first: i64 = (0..=max)
        .map(|i| numbers.iter().map(|n| (n - i).abs()).sum())
        .min()
        .unwrap();

    let second: i64 = (0..=max)
        .map(|i| {
            numbers
                .iter()
                .map(|n| (n - i).abs())
                .map(|n| n * (n + 1) / 2)
                .sum()
        })
        .min()
        .unwrap();

    println!("first  = {}", first);
    println!("second = {}", second);
}
