import json, math, strutils, algorithm, sequtils

proc wrap(node: JsonNode): JsonNode =
  if node.kind == JInt:
    result = newJArray()
    result.add(node)
  else:
    result = node

proc cmp(a, b: JsonNode): int =
  if a.kind == JInt and b.kind == JInt:
    return (a.getInt() - b.getInt()).sgn
  elif a.kind == JArray and b.kind == JArray:
    let first = a.getElems
    let second = b.getElems

    for i in 0..<min(first.len, second.len):
      let res = cmp(first[i], second[i])
      if res != 0:
        return res
    return (first.len - second.len).sgn
  else:
    return cmp(wrap(a), wrap(b))

var i = 1
var sum = 0

for line in readFile("../inputs/13").split("\n\n"):
  let parts = line.split("\n")

  if cmp(parseJson(parts[0]), parseJson(parts[1])) <= 0:
    sum += i

  inc i

echo "Part 1: ", sum

let separators = [parseJson("[[2]]"), parseJson("[[6]]")]

var items = separators.toSeq

for line in lines("../inputs/13"):
  if line == "": continue
  items.add(parseJson(line))

items.sort(cmp)

echo "Part 2: ", separators.mapIt(items.find(it) + 1).foldl(a * b)
