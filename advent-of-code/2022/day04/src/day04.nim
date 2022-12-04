import sequtils, strutils, strformat

let inputs = lines("../inputs/04")
  .toSeq
  .mapIt(it.split({',', '-'}).map(parseInt))
  .mapIt(((it[0], it[1]), (it[2], it[3])))

var partOne = 0
var partTwo = 0

for (a, b) in inputs:
  let overlap = (max(a[0], b[0]), min(a[1], b[1]))

  if overlap == a or overlap == b: partOne += 1
  if a[0] <= b[1] and b[0] <= a[1]: partTwo += 1

echo &"Part 1: {partOne}"
echo &"Part 2: {partTwo}"
