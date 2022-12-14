import strutils, sequtils, sets, math

type
  Point = tuple[x, y: int]

proc `-`(a, b: Point): Point = (a.x - b.x, a.y - b.y)
proc `+`(a, b: Point): Point = (a.x + b.x, a.y + b.y)
proc sgn(a: Point): Point = (a.x.sgn, a.y.sgn)

proc parsePoint(s: string): Point =
  let parts = s.split(',')
  (parts[0].parseInt, parts[1].parseInt)

proc parseInput(): HashSet[Point] =
  for line in lines("../inputs/14"):
    let points = line.split(" -> ").map(parsePoint)

    var point = points[0]
    for waypoint in points:
      result.incl waypoint
      while point != waypoint:
        result.incl point
        point = point + (waypoint - point).sgn

iterator flow(map: var HashSet[Point], point: Point, limit: int): Point {.closure.} =
  if not (point in map) and (point.y < limit + 2):
    for delta in [(0, 1), (-1, 1), (1, 1)]:
      let recur = flow
      for another in recur(map, point + delta, limit):
        yield another

    map.incl point
    yield point

var input = parseInput()
let limit = input.mapIt(it.y).max
var count = 0
var found = false

for point in flow(input, (500, 0), limit):
  if not found and point.y > limit:
    echo "Part 1: ", count
    found = true
  inc count

echo "Part 2: ", count
