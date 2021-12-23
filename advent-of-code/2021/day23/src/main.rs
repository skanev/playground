use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::fs;

type Cost = i32;
type Point = (usize, usize);

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Game {
    maze: Vec<Vec<u8>>,
}

fn is_amphipod(char: u8) -> bool {
    b'A' <= char && char <= b'D'
}

fn is_empty(char: u8) -> bool {
    char == b'.'
}

fn is_room(point: Point) -> bool {
    point.0 > 1
}

fn is_hallway(point: Point) -> bool {
    point.0 == 1
}

fn cost(from: Point, to: Point, char: u8) -> Cost {
    ((from.0 as Cost - to.0 as Cost).abs() + (from.1 as Cost - to.1 as Cost).abs()) * weight(char)
}

fn weight(char: u8) -> Cost {
    match char {
        b'A' => 1,
        b'B' => 10,
        b'C' => 100,
        b'D' => 1000,
        _ => unreachable!(),
    }
}

fn dedicated_room(char: u8) -> usize {
    match char {
        b'A' => 3,
        b'B' => 5,
        b'C' => 7,
        b'D' => 9,
        _ => unreachable!(),
    }
}

fn is_entrance(y: usize) -> bool {
    y == 3 || y == 5 || y == 7 || y == 9
}

impl Game {
    fn all_moves(&self) -> Vec<(Game, Cost)> {
        let mut result = vec![];

        for (x, line) in self.maze.iter().enumerate() {
            for (y, &char) in line.iter().enumerate() {
                let from = (x, y);

                if !is_amphipod(char) {
                    continue;
                }

                for to in self.possible_moves(from) {
                    result.push((self.walk(from, to), cost(from, to, char)));
                }
            }
        }

        result
    }

    fn possible_moves(&self, point: Point) -> Vec<Point> {
        let mut result = vec![];

        if is_room(point) && !self.should_stay(point) && self.can_exit(point) {
            (1..point.1)
                .rev()
                .filter(|&y| !is_entrance(y))
                .take_while(|&y| self.maze[1][y] == b'.')
                .for_each(|y| result.push((1, y)));

            (point.1..=11)
                .filter(|&y| !is_entrance(y))
                .take_while(|&y| self.maze[1][y] == b'.')
                .for_each(|y| result.push((1, y)));
        } else if is_hallway(point) {
            let char = *self.at(point);
            let y = dedicated_room(char);

            if let Some(x) = self.room_slot(char) {
                if self.can_cross(point.1, y) {
                    result.push((x, y))
                }
            }
        }

        result
    }

    fn at(&self, point: Point) -> &u8 {
        &self.maze[point.0][point.1]
    }

    fn at_mut(&mut self, point: Point) -> &mut u8 {
        &mut self.maze[point.0][point.1]
    }

    fn room_slot(&self, char: u8) -> Option<usize> {
        let y = dedicated_room(char);
        if !(2..=self.depth())
            .map(|x| *self.at((x, y)))
            .all(|c| c == b'.' || c == char)
        {
            return None;
        }

        (2..=self.depth())
            .take_while(|&x| *self.at((x, y)) == b'.')
            .last()
    }

    fn can_cross(&self, from: usize, to: usize) -> bool {
        if from < to {
            (from + 1..=to).all(|y| is_empty(*self.at((1, y))))
        } else {
            (to..from).all(|y| is_empty(*self.at((1, y))))
        }
    }

    fn depth(&self) -> usize {
        self.maze.len() - 2
    }

    fn walk(&self, from: Point, to: Point) -> Game {
        let mut new = Game {
            maze: self.maze.clone(),
        };

        *new.at_mut(to) = *self.at(from);
        *new.at_mut(from) = b'.';

        new
    }

    fn should_stay(&self, point: Point) -> bool {
        let char = *self.at(point);
        let y = dedicated_room(char);

        point.1 == y && (point.0..=self.depth()).all(|x| *self.at((x, y)) == char)
    }

    fn can_exit(&self, point: Point) -> bool {
        (2..point.0).all(|x| self.maze[x][point.1] == b'.')
    }

    fn is_solved(&self) -> bool {
        (b'A'..=b'D').all(|char| {
            let y = dedicated_room(char);
            (2..=self.depth()).all(|x| *self.at((x, y)) == char)
        })
    }
}

fn parse_input() -> Game {
    let maze = fs::read_to_string("../inputs/23")
        .unwrap()
        .lines()
        .into_iter()
        .map(|line| line.bytes().collect())
        .collect();

    Game { maze }
}

fn solve(game: Game) -> Cost {
    let mut heap: BinaryHeap<Reverse<(Cost, Game)>> = BinaryHeap::new();
    let mut costs: HashMap<Game, Cost> = HashMap::new();

    costs.insert(game.clone(), 0);
    heap.push(Reverse((0, game)));

    while let Some(Reverse((cost, game))) = heap.pop() {
        let moves = game.all_moves();

        if game.is_solved() {
            return cost;
        } else {
            for (new, delta) in moves {
                let incremental = cost + delta;
                let previous = *costs.get(&new).unwrap_or(&Cost::MAX);
                if incremental < previous {
                    costs.insert(new.clone(), incremental);
                    heap.push(Reverse((incremental, new.clone())));
                }
            }
        }
    }

    -1
}

fn unfold(game: &Game) -> Game {
    let mut maze: Vec<Vec<u8>> = vec![];

    for line in &game.maze[0..=2] {
        maze.push(line.clone());
    }

    maze.push("  #D#C#B#A#".bytes().collect());
    maze.push("  #D#B#A#C#".bytes().collect());

    for line in &game.maze[3..=4] {
        maze.push(line.clone());
    }

    Game { maze }
}

fn main() {
    let game = parse_input();
    let unfolded = unfold(&game);

    println!("first  = {}", solve(game));
    println!("second = {}", solve(unfolded));
}
