use std::fs;

fn parse_input() -> (i64, i64, i64, i64) {
    let line = fs::read_to_string("../inputs/17").unwrap();
    let mut chunks = line[15..line.len() - 1].split(", y=");
    let mut xs = chunks.next().unwrap().split("..");
    let mut ys = chunks.next().unwrap().split("..");

    let left = xs.next().unwrap().parse().unwrap();
    let right = xs.next().unwrap().parse().unwrap();
    let bottom = ys.next().unwrap().parse().unwrap();
    let top = ys.next().unwrap().parse().unwrap();

    (left, right, bottom, top)
}

fn main() {
    let (left, right, bottom, top) = parse_input();

    let mut max_height = i64::MIN;
    let mut hits = 0;

    let from = (((left as f64) * 8.0 + 1.0).sqrt() / 2.0 - 0.5).floor() as i64;

    for dx in from..=right {
        for mut dy in bottom..=-bottom {
            let mut dx = dx;
            let mut x = 0;
            let mut y = 0;

            let peak = dy * (dy + 1) / 2;

            while y >= bottom && x <= right && (dx > 0 || left <= x) {
                if left <= x && x <= right && bottom <= y && y <= top {
                    max_height = max_height.max(peak);
                    hits += 1;
                    break;
                }

                x += dx;
                y += dy;

                dx -= dx.signum();
                dy -= 1;
            }
        }
    }

    println!("first  = {}", max_height);
    println!("second = {}", hits);
}
