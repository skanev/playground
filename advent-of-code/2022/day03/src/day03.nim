import sugar, sets, sequtils, strutils, math, strformat

proc score(groups: seq[seq[string]]): int =
  groups
    .map(group => group.mapIt(it.toHashSet).foldl(a * b).toSeq[0])
    .map(item => item.ord - (if item >= 'a': 'a'.ord else: 'A'.ord - 26) + 1)
    .sum

let lines = lines("../inputs/03").toSeq

let partOne = lines.map(it => it.toSeq.distribute(2).mapIt(it.join)).score
let partTwo = countup(0, lines.len - 1, 3).toSeq.mapIt(lines[it..it+2]).score

echo &"Part 1: {partOne}"
echo &"Part 2: {partTwo}"
