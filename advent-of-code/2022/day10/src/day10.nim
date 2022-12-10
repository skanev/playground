import strutils, sequtils

iterator changes(): int =
  for line in lines("../inputs/10"):
    yield 0
    if line != "noop":
      yield line.split(" ")[1].parseInt

var cycle = 0
var register = 1
var sum = 0

var display = newSeq[array[40, bool]](6)

for n in changes():
  if (register - (cycle mod 40)).abs <= 1:
    display[cycle div 40][cycle mod 40] = true

  cycle += 1

  if cycle mod 40 == 20:
    sum += register * cycle

  register += n

echo sum

for row in display:
  echo row.mapIt(if it: "#" else: " ").join
