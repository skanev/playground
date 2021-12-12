use std::{collections::HashMap, fs};

type Graph<'a> = HashMap<&'a str, Vec<&'a str>>;

fn is_small(cave: &str) -> bool {
    cave.chars().all(|c| c.is_ascii_lowercase())
}

fn parse_input(text: &str) -> Graph {
    let mut graph: Graph = HashMap::new();

    for line in text.lines() {
        let mut parts = line.split("-");
        let from = parts.next().unwrap();
        let to = parts.next().unwrap();

        graph.entry(from).or_insert(vec![]).push(to);
        graph.entry(to).or_insert(vec![]).push(from);
    }

    graph
}

fn paths(graph: &Graph) -> (usize, usize) {
    let mut first: usize = 0;
    let mut second: usize = 0;
    let mut sofar: Vec<&str> = vec!["start"];

    fn visit<'a>(
        graph: &Graph<'a>,
        sofar: &mut Vec<&'a str>,
        twiced: bool,
        first: &mut usize,
        second: &mut usize,
    ) {
        for cave in &graph[sofar.last().unwrap()] {
            let seen = is_small(cave) && sofar.contains(cave);

            if *cave == "start" || seen && twiced {
                continue;
            } else if *cave == "end" {
                if !twiced {
                    *first += 1;
                }
                *second += 1;
            } else {
                sofar.push(cave);
                visit(graph, sofar, twiced || seen, first, second);
                sofar.pop();
            }
        }
    }

    visit(graph, &mut sofar, false, &mut first, &mut second);

    (first, second)
}

fn main() {
    let text = fs::read_to_string("../inputs/12").unwrap();
    let input = parse_input(&text);
    let (first, second) = paths(&input);

    println!("first  = {}", first);
    println!("second = {}", second);
}
