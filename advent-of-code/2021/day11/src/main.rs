use std::fs;

type Board = Vec<Vec<usize>>;

fn advance(board: &mut Board, h: usize, w: usize) -> usize {
    fn increment(board: &mut Board, i: usize, j: usize, h: usize, w: usize) {
        board[i][j] += 1;

        if board[i][j] != 10 {
            return;
        }

        for x in i.saturating_sub(1)..(i + 2).min(h) {
            for y in j.saturating_sub(1)..(j + 2).min(w) {
                increment(board, x, y, w, h);
            }
        }
    }

    for i in 0..h {
        for j in 0..w {
            increment(board, i, j, h, w)
        }
    }

    let mut flashes = 0;

    for cell in board
        .iter_mut()
        .flat_map(|line| line.iter_mut())
        .filter(|c| **c >= 10)
    {
        *cell = 0;
        flashes += 1;
    }

    flashes
}

fn first(board: &Board, h: usize, w: usize) -> usize {
    let mut board: Board = board.clone();
    let mut flashes = 0;

    for _ in 0..100 {
        flashes += advance(&mut board, h, w);
    }

    flashes
}

fn second(board: &Board, h: usize, w: usize) -> usize {
    let mut step = 0;
    let mut board = board.clone();

    loop {
        step += 1;

        if advance(&mut board, h, w) == h * w {
            break;
        }
    }

    step
}

fn main() {
    let board: Board = fs::read_to_string("../inputs/11")
        .unwrap()
        .lines()
        .map(|line| line.bytes().map(|b| (b - b'0') as usize).collect())
        .collect();

    let h = board.len();
    let w = board[0].len();

    println!("first  = {}", first(&board, h, w));
    println!("second = {}", second(&board, h, w));
}
