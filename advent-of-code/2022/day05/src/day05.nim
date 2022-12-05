import strutils, sequtils, algorithm, sugar

type
  Input = object
    stacks: seq[seq[char]]
    instructions: seq[tuple[count, source, target: int]]

proc readInput(): Input =
  let parts = readFile("../inputs/05").split("\n\n")

  let stackLines = parts[0].split("\n").reversed[1..^1]
  let numStacks = stackLines[0].len div 4 + 1

  result.stacks = repeat(newSeq[char](), numStacks)

  for i in 0..stackLines.len-1:
    for j in 0..<numStacks:
      let item = stackLines[i][j*4 + 1]
      if item != ' ': result.stacks[j].add(item)

  result.instructions = collect:
    for line in parts[1].split("\n")[0..^2]:
      let words = line.split(" ")
      (words[1].parseInt, words[3].parseInt - 1, words[5].parseInt - 1)

proc solve(input: Input, transform: seq[char] -> seq[char]): string =
  var stacks = input.stacks

  for (count, source, target) in input.instructions:
    let start = max(stacks[source].len - count, 0)
    stacks[target].add stacks[source][start..^1].transform
    stacks[source] = stacks[source][0..<start]

  stacks.mapIt(it[^1]).join

echo "Part 1: ", readInput().solve(x => x.reversed)
echo "Part 2: ", readInput().solve(x => x)
