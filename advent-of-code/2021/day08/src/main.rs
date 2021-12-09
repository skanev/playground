use std::fs::read_to_string;

#[derive(Debug)]
struct InputLine {
    patterns: Vec<String>,
    output: Vec<String>,
}

fn parse_input() -> Vec<InputLine> {
    read_to_string("../inputs/08")
        .unwrap()
        .lines()
        .map(|line| {
            let parts = line.split(" | ").collect::<Vec<_>>();

            let patterns = parts[0].split(" ").map(|s| s.to_string()).collect();
            let output = parts[1].split(" ").map(|s| s.to_string()).collect();

            InputLine { patterns, output }
        })
        .collect()
}

fn bits(n: u8) -> u32 {
    n.count_ones()
}

fn bitset<'a>(number: &'a str) -> u8 {
    number
        .as_bytes()
        .iter()
        .map(|c| 1 << (c - b'a'))
        .fold(0, |a, b| a | b)
}

fn consume<F>(patterns: &mut Vec<u8>, f: F) -> u8
where
    F: Fn(u8) -> bool,
{
    patterns.swap_remove(patterns.iter().position(|n| f(*n)).unwrap())
}

fn decrypt(line: &InputLine) -> usize {
    let mut left: Vec<u8> = line.patterns.iter().map(|str| bitset(str)).collect();
    let mut m = [0_u8; 10];

    m[1] = consume(&mut left, |p| bits(p) == 2);
    m[4] = consume(&mut left, |p| bits(p) == 4);
    m[7] = consume(&mut left, |p| bits(p) == 3);
    m[8] = consume(&mut left, |p| bits(p) == 7);
    m[3] = consume(&mut left, |p| bits(p) == 5 && m[7] & p == m[7]);
    m[6] = consume(&mut left, |p| bits(p) == 6 && m[1] & p != m[1]);
    m[2] = consume(&mut left, |p| bits(p) == 5 && m[8] & !m[6] & p != 0);
    m[5] = consume(&mut left, |p| bits(p) == 5);
    m[9] = consume(&mut left, |p| m[8] & !p & m[4] == 0);
    m[0] = consume(&mut left, |_| true);

    line.output
        .iter()
        .map(|number| bitset(number))
        .map(|target| m.iter().position(|pattern| *pattern == target).unwrap())
        .fold(0, |accumulator, digit| accumulator * 10 + digit)
}

fn main() {
    let first: usize = parse_input()
        .iter()
        .flat_map(|line| &line.output)
        .map(|seq| seq.len())
        .filter(|&n| n != 5 && n != 6)
        .count();

    let second: usize = parse_input().iter().map(|line| decrypt(line)).sum();

    println!("first  = {}", first);
    println!("second = {}", second);
}
