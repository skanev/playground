import strutils, sequtils, sets, sequtils, sugar, math

type
  Point = tuple[x, y, z: int]

proc within(p: Point, lower: int, higher: int): bool =
  lower <= p.x and p.x <= higher and
    lower <= p.y and p.y <= higher and
    lower <= p.z and p.z <= higher

proc sides(p: Point): seq[Point] =
  @[
    (p.x - 1, p.y, p.z),
    (p.x + 1, p.y, p.z),
    (p.x, p.y - 1, p.z),
    (p.x, p.y + 1, p.z),
    (p.x, p.y, p.z - 1),
    (p.x, p.y, p.z + 1),
  ]

var cubes: HashSet[Point]

for line in lines("../inputs/18"):
  let parts = line.split(',').mapIt(it.parseInt)
  cubes.incl (x: parts[0], y: parts[1], z: parts[2])

let lower = cubes.mapIt([it.x, it.y, it.z].min).min - 1
let higher = cubes.mapIt([it.x, it.y, it.z].max).max + 1

var left: seq[Point] = @[(x: lower, y: lower, z: lower)]
var seen: HashSet[Point]
var count = 0

while left.len > 0:
  let point = left.pop

  if point in seen:
    continue

  seen.incl point

  for side in sides(point):
    if side in cubes:
      inc count
    elif side notin seen and within(side, lower, higher):
      left.add side


echo "Part 1: ", cubes.toSeq.map(cube => cube.sides.countIt(it notin cubes)).sum
echo "Part 2: ", count
