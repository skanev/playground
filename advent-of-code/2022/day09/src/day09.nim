import strutils, math, sets, strformat

type
  Point = tuple[x, y: int]

proc `+`(a, b: Point): Point = (a.x + b.x, a.y + b.y)
proc `-`(a, b: Point): Point = (a.x - b.x, a.y - b.y)
proc sgn(a: Point): Point = (sgn(a.x), sgn(a.y))

iterator directions(): string =
  for line in lines("../inputs/09"):
    let parts = line.split(" ")

    for _ in 0..<parts[1].parseInt:
      yield parts[0]

var rope: array[10, Point]
var one = initHashSet[Point]()
var two = initHashSet[Point]()

for direction in directions():
  case direction:
    of "L": rope[0].y -= 1
    of "R": rope[0].y += 1
    of "U": rope[0].x -= 1
    of "D": rope[0].x += 1
    else: raise

  for i in 1..9:
    let delta = rope[i - 1] - rope[i]
    if delta.x.abs > 1 or delta.y.abs > 1:
      rope[i] = rope[i] + delta.sgn

  one.incl(rope[1])
  two.incl(rope[^1])

echo &"Part 1: {one.len}"
echo &"Part 2: {two.len}"
