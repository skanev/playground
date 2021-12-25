use std::fs;

fn main() {
    let mut map: Vec<Vec<u8>> = fs::read_to_string("../inputs/25")
        .unwrap()
        .lines()
        .map(|line| line.bytes().collect())
        .collect();

    let mut steps = 0;
    let h = map.len();
    let w = map[0].len();

    loop {
        steps += 1;

        let mut horizontal = vec![];

        for x in 0..h {
            for y in 0..w {
                let adjancent = (y + 1) % w;
                if map[x][y] == b'>' && map[x][adjancent] == b'.' {
                    horizontal.push((x, y, adjancent));
                }
            }
        }

        for &(x, y, a) in &horizontal {
            map[x][y] = b'.';
            map[x][a] = b'>';
        }

        let mut vertical = vec![];

        for x in 0..h {
            for y in 0..w {
                let adjancent = (x + 1) % h;
                if map[x][y] == b'v' && map[adjancent][y] == b'.' {
                    vertical.push((x, y, adjancent));
                }
            }
        }

        for &(x, y, a) in &vertical {
            map[x][y] = b'.';
            map[a][y] = b'v';
        }

        if vertical.is_empty() && horizontal.is_empty() {
            break;
        }
    }

    println!("final = {}", steps);
}
