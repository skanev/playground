use std::fs;

struct Cell {
    number: i64,
    marked: bool,
}

struct Board {
    rows: Vec<Vec<Cell>>,
    score: Option<i64>,
}

impl Board {
    fn iter(&self) -> impl Iterator<Item=&Cell> {
        self.rows.iter().flat_map(|row| row.iter())
    }

    fn iter_mut(&mut self) -> impl Iterator<Item=&mut Cell> {
        self.rows.iter_mut().flat_map(|row| row.iter_mut())
    }

    fn mark(&mut self, number: i64) {
        for cell in self.iter_mut().filter(|c| c.number == number) {
            cell.marked = true
        }
    }

    fn is_winning(&self) -> bool {
        self.rows.iter().any(|row| row.iter().all(|c| c.marked))
            || (0..4).any(|col| self.rows.iter().all(|row| row[col].marked))
    }

    fn unmarked_sum(&self) -> i64 {
        self.iter().map(|c| c.number).sum()
    }
}

fn parse_input() -> (Vec<i64>, Vec<Board>) {
    let lines = fs::read_to_string("../inputs/04")
        .unwrap()
        .lines()
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    let numbers = lines[0]
        .split(",")
        .map(|n| n.parse::<i64>().unwrap())
        .collect();

    let boards = lines[2..]
        .chunks(6)
        .map(|rows| Board {
            score: None,
            rows: rows
                .iter()
                .take(5)
                .map(|line| {
                    line.split_ascii_whitespace()
                        .map(|x| Cell {
                            number: x.parse().unwrap(),
                            marked: false,
                        })
                        .collect()
                })
                .collect(),
        })
        .collect();

    (numbers, boards)
}

fn main() {
    let (numbers, mut boards) = parse_input();

    let mut scores: Vec<i64> = vec![];

    for number in numbers {
        for board in boards.iter_mut().filter(|b| b.score == None) {
            board.mark(number);

            if board.is_winning() {
                let score = board.unmarked_sum() * number;
                board.score = Some(score);
                scores.push(score)
            }
        }
    }

    println!("first = {}", scores.first().unwrap());
    println!("second = {}", scores.last().unwrap());
}
