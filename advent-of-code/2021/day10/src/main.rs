use std::fs;

fn score(input: &str) -> (usize, usize) {
    let mut stack: Vec<char> = vec![];

    for char in input.chars() {
        match char {
            '(' | '{' | '[' | '<' => stack.push(char),
            next => match (stack.last().unwrap_or(&' '), next) {
                ('(', ')') | ('{', '}') | ('[', ']') | ('<', '>') => {
                    stack.pop();
                }
                (_, ')') => return (3, 0),
                (_, ']') => return (57, 0),
                (_, '}') => return (1197, 0),
                (_, '>') => return (25137, 0),
                _ => panic!(),
            },
        }
    }

    let score = stack.iter().rev().fold(0, |score, char| {
        5 * score + " ([{<".chars().position(|c| c == *char).unwrap()
    });

    (0, score)
}

fn main() {
    let results: Vec<(usize, usize)> = fs::read_to_string("../inputs/10")
        .unwrap()
        .lines()
        .map(|line| score(line))
        .collect();

    let first: usize = results.iter().map(|s| s.0).sum();

    let mut scores: Vec<usize> = results.iter().map(|s| s.1).filter(|&c| c != 0).collect();
    scores.sort();

    let second = scores[scores.len() / 2];

    println!("first  = {}", first);
    println!("second = {}", second);
}
