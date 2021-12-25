use std::fs;

type Num = i32;

fn main() {
    let digits = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    let fns: Vec<(Num, Num, Num)> = fs::read_to_string("../inputs/24")
        .unwrap()
        .lines()
        .map(|line| line.split(" ").nth(2).unwrap_or(""))
        .collect::<Vec<_>>()
        .chunks(18)
        .map(|block| (block[4].parse().unwrap(), block[5].parse().unwrap(), block[15].parse().unwrap()))
        .collect();

    let mut stack: Vec<(usize, Num)> = vec![];
    let mut pattern: Vec<Vec<Num>> = vec![vec![]; 14];

    for (i, (a, b, c)) in fns.into_iter().enumerate() {
        if a == 1 {
            stack.push((i, c));
        } else {
            let (dep, other) = stack.pop().unwrap();
            for (x, y) in digits.iter().flat_map(|&x| digits.map(|y| (x, y))).filter(|&(x, y)| x == other + y + b) {
                pattern[i].push(x);
                pattern[dep].push(y);
            }
        }
    }

    println!("first  = {}", pattern.iter().map(|p| p.iter().last().unwrap().to_string()).collect::<Vec<_>>().join(""));
    println!("second = {}", pattern.iter().map(|p| p.iter().nth(0).unwrap().to_string()).collect::<Vec<_>>().join(""));
}
