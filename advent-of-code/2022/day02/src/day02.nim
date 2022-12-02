import strformat

var partOne = 0
var partTwo = 0

for line in lines("../inputs/02"):
  let first = int(line[0]) - int('A')
  let second = int(line[2]) - int('X')

  partOne += ((second - first + 4) mod 3) * 3 + second + 1
  partTwo += second * 3 + (first + second + 2) mod 3 + 1

echo &"Part 1: {partOne}"
echo &"Part 2: {partTwo}"
