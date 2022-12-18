import std/enumerate, sets, sequtils, tables

type
  Point = tuple[x: int, y: int]

let shapes = [
  @[(0, 0), (1, 0), (2, 0), (3, 0)],
  @[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
  @[(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
  @[(0, 0), (0, 1), (0, 2), (0, 3)],
  @[(0, 0), (1, 0), (0, 1), (1, 1)],
]

proc `+`(a, b: Point): Point = (a.x + b.x, a.y + b.y)


proc fingerprint(cave: HashSet[Point], top: int): string =
  for y in countdown(top, top - 30):
    for x in 0 .. 6:
      result.add(if (x, y) in cave: '#' else: '.')

var wind: seq[Point]

for c in readFile("../inputs/17"):
  case c:
    of '<': wind.add((-1, 0))
    of '>': wind.add((1, 0))
    of '\n': discard
    else: raise newException(ValueError, "Invalid character: " & $c)

var rock: seq[Point] = shapes[0].mapIt(it + (2, 4))
var cave: HashSet[Point]
var fallen = 1
var foundPhase = false
var gain: int64 = 0
var left = 0
var seen: Table[(int, int, string), (int, int)]

proc move(point: seq[Point], delta: Point): seq[Point] =
  let moved = point.mapIt(it + delta)

  if moved.allIt(it notin cave and 0 <= it.x and it.x <= 6 and 0 <= it.y):
    return moved
  else:
    return point

block solution:
  while true:
    for (beat, delta) in enumerate(wind):
      rock = rock.move((0, -1))
      rock = rock.move(delta)

      if rock.move((0, -1)) != rock:
        continue

      for pebble in rock:
        cave.incl pebble

      let height = cave.mapIt(it.y).max + 1
      let memo = (fallen mod shapes.len, beat, fingerprint(cave, height))

      if fallen == 2022:
        echo "Part 1: ", height

      if foundPhase:
        dec left
        if left == 0: echo "Part 2: ", height + gain
      elif seen.hasKey(memo):
        let (a, b) = seen[memo]
        let phase = fallen - a
        let remaining = 1_000_000_000_000 - fallen
        let ha = (remaining div phase).int

        left = int(remaining mod phase)
        gain = ha * (height - b)
        foundPhase = true
      else:
        seen[memo] = (fallen, height)

      rock = shapes[fallen mod shapes.len].mapIt(it + (2, height + 4))
      inc fallen

      if left < 0 and fallen > 2022:
        break solution
