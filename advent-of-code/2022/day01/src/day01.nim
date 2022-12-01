import strutils, sequtils, strformat, algorithm

let data = readFile("../inputs/01")

proc calories(): seq[int] =
  for chunk in data.split("\n\n"):
    var chunk = chunk
    chunk.removeSuffix
    result.add(chunk.split("\n").map(parseInt).foldl(a + b))

  result.sort(Descending)

let part1 = calories()[0]
let part2 = calories()[0..2].foldl(a + b)

echo fmt"Part 1: {part1}"
echo fmt"Part 2: {part2}"
