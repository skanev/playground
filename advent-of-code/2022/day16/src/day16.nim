import re, tables, strutils, sequtils

type
  Index = range[0..63]

var indices: Table[string, Index]
var rates: array[low(Index)..high(Index), int]
var paths: array[low(Index)..high(Index), seq[Index]]

var index: Index = 0
var pathNames: Table[string, seq[string]]

for line in lines("../inputs/16"):
  let name = line[6..7]
  let rate = line.findAll(re"\d+")[0].parseInt
  let rooms = line.split(re"valves? ")[1].split(", ")

  rates[index] = rate
  pathNames[name] = rooms
  indices[name] = index

  inc index

for (name, rooms) in pathNames.pairs:
  paths[indices[name]] = rooms.mapIt(indices[it])

proc flow(open: set[Index]): int =
  for i in open:
    inc result, rates[i]

type Memo = tuple[a, b: Index, time: int]

iterator possible(position: Index, open: set[Index], valid: bool): (Index, set[Index]) =
  if valid:
    if not(position in open) and rates[position] > 0:
      yield (position, open + {position})

    for next in paths[position]:
      yield (next, open)
  else:
    yield (position, open)

proc solve(steps: int, moveElephant: bool): int =
  var best = 0
  var seen: Table[Memo, int]

  proc go(time: int, one: Index, two: Index, score: int, open: set[Index]) =
    if time == 1:
      best = best.max(score)
      return

    if seen.hasKey((one, two, time)) and seen[(one, two, time)] >= score:
      return

    seen[(one, two, time)] = score

    for (one, open) in possible(one, open, moveElephant):
      for (two, open) in possible(two, open, true):
        go time - 1, one, two, score + flow(open), open

  go steps, indices["AA"], indices["AA"], 0, {}

  return best

echo "Part 1: ", solve(30, false)
echo "Part 2: ", solve(26, true)
