// This is significantly more complicated than it needs to be, but I wanted to play a bit with Rust
// and do the following:
//
// * Parse the input while streaming it, instead of loading it all, converting it to packets and
//   evaluating it afterwards.
// * Evaluate each packet as early as possible (e.g. before the parent subpacket has finished
//   parsing).
// * Have a single abstraction that can both add up the version numbers and evaluate the packets
// * Convert the file to a stream of bits by massively abusing iterators and without creating a
//   custom struct that implements Iterator (hence the Boxing and the dyns).
// * Alias all the numeric types I use so they can easily be changed (e.g. bits, versions, type
//   ids, literal values).
// * Have a parse_number() function that is generic on its return type.
//
// This is very unnecessary and in places uncrustacean, but I did learn a bunch of stuff while
// doing it.
use std::fs::File;
use std::io::Read;
use std::ops::{Add, Shl};

type Bit = u8;
type LiteralValue = u64;
type Version = u8;
type TypeId = u8;
type Result = i64;

#[derive(Debug)]
enum Packet<T> {
    Literal(Version, LiteralValue),
    Operator(Version, TypeId, Vec<T>),
}

fn bits(char: u8) -> [Bit; 4] {
    let num = match char {
        (b'0'..=b'9') => char - b'0',
        (b'A'..=b'F') => char - b'A' + 10,
        _ => panic!(),
    };
    let mut result = [0; 4];

    for i in 0..4 {
        if num & (1 << i) != 0 {
            result[3 - i] = 1
        }
    }

    result
}

fn read_number<N, I>(stream: &mut I, size: usize) -> N
where
    N: Shl<usize, Output = N> + Add<Output = N> + From<Bit> + Default,
    I: Iterator<Item = Bit>,
{
    stream
        .take(size)
        .fold(N::default(), |a, b| (a << 1) + N::from(b))
}

fn read_literal<I>(stream: &mut I) -> LiteralValue
where
    I: Iterator<Item = Bit>,
{
    let mut number = 0;

    loop {
        let chunk: LiteralValue = read_number(stream, 5);
        number <<= 4;

        if chunk < 16 {
            number += chunk;
            break;
        } else {
            number += chunk - 16;
        }
    }

    number
}

fn read_packet<T>(mut stream: &mut dyn Iterator<Item = Bit>, eval: fn(Packet<T>) -> T) -> T {
    let version = read_number(&mut stream, 3);
    let type_id = read_number(&mut stream, 3);

    if type_id == 4 {
        eval(Packet::Literal(version, read_literal(&mut stream)))
    } else {
        let length_type_id: u8 = read_number(&mut stream, 1);
        let mut subpackets = vec![];

        if length_type_id == 0 {
            let length = read_number(&mut stream, 15);
            let mut substream = stream.take(length).peekable();

            while substream.peek() != None {
                subpackets.push(read_packet(&mut substream, eval));
            }

            eval(Packet::Operator(version, type_id, subpackets))
        } else {
            for _ in 0..read_number(&mut stream, 11) {
                subpackets.push(read_packet(&mut stream, eval));
            }
            eval(Packet::Operator(version, type_id, subpackets))
        }
    }
}

fn eval(packet: Packet<Result>) -> Result {
    match packet {
        Packet::Literal(_, val) => val as Result,
        Packet::Operator(_, code, vals) => match code {
            0 => vals.iter().sum(),
            1 => vals.iter().product(),
            2 => vals.iter().min().unwrap().clone(),
            3 => vals.iter().max().unwrap().clone(),
            5 => (vals[0] > vals[1]) as Result,
            6 => (vals[0] < vals[1]) as Result,
            7 => (vals[0] == vals[1]) as Result,
            _ => panic!(),
        },
    }
}

fn stream_input() -> impl Iterator<Item = Bit> {
    File::open("../inputs/16")
        .unwrap()
        .bytes()
        .map(|b| b.unwrap())
        .take_while(|&b| b != b'\n')
        .flat_map(|b| bits(b))
}

fn main() {
    let first: u64 = read_packet(&mut stream_input(), |packet| match packet {
        Packet::Literal(version, _) => version as u64,
        Packet::Operator(version, _, subpackets) => version as u64 + subpackets.iter().sum::<u64>(),
    });

    let second = read_packet(&mut stream_input(), eval);

    println!("first  = {}", first);
    println!("second = {}", second);
}
