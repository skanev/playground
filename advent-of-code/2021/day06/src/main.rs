use std::fs::read_to_string;

fn parse_input() -> Vec<usize> {
    let input = read_to_string("../inputs/06").unwrap();
    input[0..input.len()-1].split(",").map(|chunk| chunk.parse().unwrap()).collect()
}

fn iterate(numbers: &[usize; 9], iterations: usize) -> usize {
    let mut counts = numbers.clone();

    for _ in 0..iterations {
        let born = counts[0];
        for i in 1..=8 {
            counts[i - 1] = counts[i]
        }
        counts[8] = born;
        counts[6] += born;
    }

    counts.iter().sum()
}

fn main() {
    let numbers = parse_input();
    let mut counts: [usize; 9] = [0; 9];

    for number in numbers {
        counts[number] += 1
    }

    println!("first  = {}", iterate(&counts, 80));
    println!("second = {}", iterate(&counts, 256));
}
