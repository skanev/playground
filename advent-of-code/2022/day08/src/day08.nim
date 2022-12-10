import algorithm, sequtils, strformat, sugar

let grid = lines("../inputs/08").toSeq.map(line => line.toSeq.mapIt(it.int - '0'.int))

var visible = 0
var optimal = 0

for x in 0..<grid.len:
  for y in 0..<grid[0].len:
    let height = grid[x][y]

    let lines = [
      grid[x][0..<y],
      grid[x][y+1..^1].reversed,
      (0..<x).mapIt(grid[it][y]),
      (x+1..<grid.len).mapIt(grid[it][y]).reversed,
    ]

    if lines.any(line => line.allIt(it < height)):
      inc visible

    proc reach(line: seq[int]): int =
      for tree in line:
        inc result
        if tree >= height: break

    optimal = lines.mapIt(reach(it.reversed)).foldl(a * b).max(optimal)

echo &"Part 1: {visible}"
echo &"Part 2: {optimal}"
