import sequtils, strutils, deques

var map: seq[seq[char]]
var start: (int, int)
var target: (int, int)
var distance: seq[seq[int]]

var x = 0
for line in lines("../inputs/12"):
  distance.add repeat(-1, line.len)

  if line.contains('S'):
    start = (x, line.find('S'))

  if line.contains('E'):
    target = (x, line.find('E'))

  map.add(cast[seq[char]](line.replace("E", "z").replace("S", "a")))
  inc x

let height = map.len
let width = map[0].len

var left = [(target, 0)].toDeque

while left.len > 0:
  let (point, steps) = left.popFirst
  let (x, y) = point

  if distance[x][y] != -1 and distance[x][y] <= steps:
    continue

  distance[x][y] = steps

  for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
    let (nx, ny) = (x + dx, y + dy)
    if 0 <= nx and nx < height and 0 <= ny and ny < width and map[nx][ny].ord + 1 >= map[x][y].ord:
      left.addLast(((nx, ny), steps + 1))

let first = distance[start[0]][start[1]]
var second = first

for x in 0..<height:
  for y in 0..<width:
    if map[x][y] == 'a' and distance[x][y] != -1:
      second = min(second, distance[x][y])

echo "Part 1: ", first
echo "Part 2: ", second
