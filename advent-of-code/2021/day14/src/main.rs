use std::{collections::HashMap, fs};

type Pair = (u8, u8);
type RuleSet = HashMap<Pair, u8>;
type Histogram = HashMap<Pair, usize>;

fn expand(pairs: Histogram, rules: &RuleSet) -> Histogram {
    let mut result: HashMap<Pair, usize> = HashMap::new();

    for (pair, count) in pairs {
        let middle = rules.get(&pair).unwrap();

        let first = result.entry((pair.0, *middle)).or_insert(0);
        *first += count;

        let second = result.entry((*middle, pair.1)).or_insert(0);
        *second += count;
    }

    result
}

fn parse_input() -> (String, RuleSet) {
    let text = fs::read_to_string("../inputs/14").unwrap();
    let mut parts = text.split("\n\n");
    let polymer = parts.next().unwrap().to_string();
    let rules = parts
        .next()
        .unwrap()
        .lines()
        .map(|line| {
            let chunks: Vec<&str> = line.split(" -> ").collect();
            let a = chunks[0].as_bytes()[0];
            let b = chunks[0].as_bytes()[1];
            let c = chunks[1].as_bytes()[0];
            ((a, b), c)
        })
        .collect();

    (polymer, rules)
}

fn histogram(polymer: &str) -> Histogram {
    let mut histogram = HashMap::new();

    for window in polymer.as_bytes().windows(2) {
        let pair = (window[0], window[1]);
        let count = histogram.entry(pair).or_insert(0);
        *count += 1;
    }

    histogram
}

fn solve(polymer: &str, rules: &RuleSet, iterations: usize) -> usize {
    let mut pairs = histogram(&polymer);

    for _ in 0..iterations {
        pairs = expand(pairs, &rules);
    }

    let mut counts: HashMap<u8, usize> = HashMap::new();

    for (pair, count) in pairs {
        let sofar = counts.entry(pair.0).or_insert(0);
        *sofar += count;
    }

    let last = counts.entry(polymer.bytes().last().unwrap()).or_insert(0);
    *last += 1;

    let min = counts.values().min().map(|s| s.to_owned()).unwrap_or(0);
    let max = counts.values().max().map(|s| s.to_owned()).unwrap_or(0);

    max - min
}

fn main() {
    let (polymer, rules) = parse_input();

    println!("first  = {}", solve(&polymer, &rules, 10));
    println!("second = {}", solve(&polymer, &rules, 40));
}
