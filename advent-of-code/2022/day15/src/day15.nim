import re, strutils, sequtils, algorithm, options, math, sugar

const LIMIT = 4_000_000

type
  Point = tuple[x, y: int]
  Sensor = tuple[location: Point, distance: int]
  Span = tuple[a, b: int]

proc manhattan(p1, p2: Point): int = abs(p1.x - p2.x) + abs(p1.y - p2.y)

proc `&`(x, y: Span): bool = x.a <= y.b and y.a <= x.b
proc `in`(x: int, span: Span): bool = span.a <= x and x <= span.b
proc len(span: Span): int = span.b - span.a + 1

proc parseInput(): tuple[sensors: seq[Sensor], beacons: seq[Point]] =
  for line in lines("../inputs/15"):
    let numbers = line.findAll(re"-?\d+").map(parseInt)
    let location = (x: numbers[0], y: numbers[1])
    let beacon = (x: numbers[2], y: numbers[3])

    result.sensors.add (location: location, distance: manhattan(location, beacon))
    result.beacons.add beacon

proc project(sensor: Sensor, y: int): Option[Span] =
  let point = (x: sensor.location.x, y: y)
  let distance = manhattan(point, sensor.location)

  if distance > sensor.distance:
    return none(Span)

  let delta = sensor.distance - distance

  return some((a: point.x - delta, b: point.x + delta))

proc compact(spans: openarray[Span]): seq[Span] =
  var left = spans.toSeq

  while left.len > 0:
    var span = left.pop
    var i = 0

    while i < left.len:
      if span & left[i]:
        span = (min(span.a, left[i].a), max(span.b, left[i].b))
        left.delete(i)
        i = 0
      else:
        inc i

    result.add span

proc nonBeacons(sensors: seq[Sensor], beacons: openarray[Point], y: int): int =
  let spans = sensors.mapIt(project(it, y)).filterIt(it.isSome).mapIt(it.get).compact
  let overlapped = beacons.filter(b => b.y == y and spans.anyIt(b.x in it)).deduplicate

  return spans.mapIt(it.len).sum - overlapped.len

proc tuningFrequency(sensors: openarray[Sensor], limit: int): int =
  var sorted = sensors.sortedByIt((it.location.x, it.distance))

  for y in 0..LIMIT:
    var x = 0

    for sensor in sorted:
      if manhattan((x: x, y: y), sensor.location) <= sensor.distance:
        x = sensor.location.x + sensor.distance - abs(sensor.location.y - y) + 1

    if x <= LIMIT:
      return x * 4_000_000 + y

let (sensors, beacons) = parseInput()

echo "Part 1: ", nonBeacons(sensors, beacons, LIMIT div 2)
echo "Part 2: ", tuningFrequency(sensors, LIMIT)
