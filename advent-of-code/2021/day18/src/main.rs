use std::fs;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Part {
    Open,
    Close,
    Comma,
    Number(u8),
}

use Part::*;

type Snailfish = Vec<Part>;

fn parse_number(text: &str) -> Snailfish {
    text.bytes()
        .map(|char| match char {
            b'[' => Open,
            b']' => Close,
            b',' => Comma,
            b'0'..=b'9' => Number(char - b'0'),
            _ => unreachable!(),
        })
        .collect()
}

fn explode(snailfish: Snailfish) -> Snailfish {
    let mut result = vec![];
    let mut depth = 0;

    let mut remaining = snailfish.into_iter();

    while let Some(part) = remaining.next() {
        match part {
            Open => depth += 1,
            Close => depth -= 1,
            Number(first) if depth > 4 => {
                assert_eq!(remaining.next(), Some(Comma));

                let second: u8;
                match remaining.next() {
                    Some(Number(n)) => second = n,
                    _ => unreachable!(),
                }

                assert_eq!(remaining.next(), Some(Close));

                for previous in result.iter_mut().rev() {
                    match previous {
                        Number(val) => {
                            *val += first;
                            break;
                        }
                        _ => {}
                    }
                }

                assert_eq!(result.pop(), Some(Open));

                result.push(Number(0));

                while let Some(part) = remaining.next() {
                    if let Number(n) = part {
                        result.push(Number(n + second));
                        break;
                    } else {
                        result.push(part);
                    }
                }

                while let Some(part) = remaining.next() {
                    result.push(part)
                }

                break;
            }
            _ => (),
        }

        result.push(part.clone())
    }

    result
}

fn split(snailfish: Snailfish) -> Snailfish {
    let mut result = vec![];
    let mut done = false;

    for part in snailfish {
        match part {
            Number(n) if n >= 10 && !done => {
                let a = n / 2;
                let b = n - a;

                result.push(Open);
                result.push(Number(a));
                result.push(Comma);
                result.push(Number(b));
                result.push(Close);
                done = true;
            }
            _ => result.push(part),
        }
    }

    result
}

fn reduce(snailfish: Snailfish) -> Snailfish {
    let mut previous;
    let mut result = snailfish;
    loop {
        previous = result.len();
        result = explode(result);

        while result.len() != previous {
            previous = result.len();
            result = explode(result);
        }

        result = split(result);

        if previous == result.len() {
            return result;
        }
    }
}

fn add(mut first: Snailfish, mut second: Snailfish) -> Snailfish {
    let mut result = vec![];
    result.push(Open);
    result.append(&mut first);
    result.push(Comma);
    result.append(&mut second);
    result.push(Close);

    reduce(result)
}

fn magnitude(snailfish: &Snailfish) -> u64 {
    let mut stack: Vec<u64> = vec![];

    for part in snailfish {
        match part {
            Open => stack.push(3),
            Close => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                let c = stack.pop().unwrap();
                stack.push(a * b + c);
            }
            Comma => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(a * b);
                stack.push(2);
            }
            Number(n) => stack.push(*n as u64),
        }
    }

    stack.pop().unwrap()
}

fn parse_input() -> Vec<Snailfish> {
    fs::read_to_string("../inputs/18")
        .unwrap()
        .lines()
        .map(|s| parse_number(s))
        .collect()
}

fn main() {
    let first = parse_input().into_iter().reduce(|a, b| add(a, b)).unwrap();

    let first = magnitude(&first);

    let mut second = 0;
    let numbers = parse_input();

    for a in numbers.iter() {
        for b in numbers.iter() {
            if a == b {
                continue;
            }

            let sum = add(a.clone(), b.clone());
            second = second.max(magnitude(&sum));
        }
    }

    println!("first  = {}", first);
    println!("second = {}", second);
}
