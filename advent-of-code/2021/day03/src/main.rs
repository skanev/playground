use std::fs;

fn main() {
    println!("first = {:?}", first());
    println!("second = {:?}", second());
}

fn input() -> Vec<String> {
    fs::read_to_string("../inputs/03")
        .unwrap()
        .lines()
        .map(|line| line.to_string())
        .collect()
}

fn ones(numbers: &Vec<String>) -> Vec<usize> {
    let length = numbers[0].len();
    let mut counts: Vec<usize> = vec![0; length];

    for number in numbers {
        for (i, _) in number.chars().enumerate().filter(|(_, b)| *b == '1') {
            counts[i] += 1
        }
    }

    counts
}

fn decimal(digits: &Vec<usize>) -> usize {
    digits
        .iter()
        .rev()
        .enumerate()
        .fold(0, |a, (i, &b)| a + (b << i))
}

fn first() -> usize {
    let numbers = input();
    let n = numbers.len();
    let counts = ones(&numbers);
    let most_common_bits = counts
        .iter()
        .map(|&count| if count > n / 2 { 1 } else { 0 })
        .collect::<Vec<_>>();
    let least_common_bits = most_common_bits.iter().map(|&b| 1 - b).collect::<Vec<_>>();

    decimal(&most_common_bits) * decimal(&least_common_bits)
}

fn filter<F>(numbers: Vec<String>, position: usize, f: F) -> Vec<String>
where
    F: Fn(usize, usize) -> bool,
{
    let count = ones(&numbers)[position];
    let target = if f(count, numbers.len()) { b'1' } else { b'0' };

    numbers
        .into_iter()
        .filter(|number| number.as_bytes()[position] == target)
        .collect()
}

fn iterate<F>(numbers: &Vec<String>, f: F) -> usize
where
    F: Fn(usize, usize) -> bool,
{
    let mut numbers = numbers.clone();
    let w = numbers[0].len();

    for i in (0..w).cycle() {
        if numbers.len() == 1 {
            return usize::from_str_radix(&numbers[0], 2).unwrap();
        } else {
            numbers = filter(numbers, i, &f);
        }
    }

    panic!()
}

fn second() -> usize {
    let numbers = input();

    iterate(&numbers, |count, total| count * 2 >= total)
        * iterate(&numbers, |count, total| count * 2 < total)
}
