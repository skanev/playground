use std::{collections::HashMap, fs};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct Game {
    positions: [u16; 2],
    scores: [u16; 2],
    turn: usize,
}

impl Game {
    fn from(positions: (u16, u16)) -> Game {
        Game {
            positions: [positions.0, positions.1],
            scores: [0, 0],
            turn: 0,
        }
    }
    fn is_finished(&self, winning_score: u16) -> bool {
        self.scores.iter().any(|score| *score >= winning_score)
    }

    fn leader(&self) -> usize {
        if self.scores[0] > self.scores[1] {
            0
        } else {
            1
        }
    }

    fn play(&self, roll: u16) -> Game {
        let new = increment_position(self.positions[self.turn], roll);
        let mut scores = self.scores.clone();
        let mut positions = self.positions.clone();
        positions[self.turn] = new;
        scores[self.turn] += new;
        Game {
            positions,
            scores,
            turn: 1 - self.turn,
        }
    }
}

fn parse_input() -> (u16, u16) {
    let numbers: Vec<u16> = fs::read_to_string("../inputs/21")
        .unwrap()
        .lines()
        .map(|line| line.split(": ").nth(1).unwrap().parse().unwrap())
        .collect();
    (numbers[0], numbers[1])
}

fn increment_position(position: u16, count: u16) -> u16 {
    (position + count - 1) % 10 + 1
}

fn first(positions: (u16, u16)) -> u64 {
    let mut game = Game::from(positions);

    let mut die = (1..=100).cycle().enumerate();

    while !game.is_finished(1000) {
        let mut roll = 0;
        roll += die.next().unwrap().1;
        roll += die.next().unwrap().1;
        roll += die.next().unwrap().1;
        game = game.play(roll);
    }

    let winner = game.leader();
    let rolls = die.next().unwrap().0 as u64;

    (game.scores[1 - winner] as u64) * rolls
}

fn second(positions: (u16, u16)) -> u64 {
    let mut unfinished: HashMap<Game, u64> = HashMap::new();
    let mut finished: HashMap<Game, u64> = HashMap::new();

    let dirac = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)];

    unfinished.insert(Game::from(positions), 1);

    while unfinished.len() != 0 {
        let mut next = HashMap::new();

        for (game, count) in unfinished {
            for (roll, times) in dirac {
                let result = game.play(roll);

                if result.is_finished(21) {
                    *finished.entry(result).or_insert(0) += times * count;
                } else {
                    *next.entry(result).or_insert(0) += times * count;
                }
            }
        }

        unfinished = next;
    }

    let mut wins: [u64; 2] = [0, 0];

    for (game, count) in finished {
        wins[game.leader()] += count;
    }

    wins.into_iter().max().unwrap()
}

fn main() {
    let input = parse_input();

    println!("first  = {}", first(input));
    println!("second = {}", second(input));
}
