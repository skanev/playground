import strutils, tables, sugar, sequtils, math, strformat

var cwd: seq[string] = @[""]
var sizes = {"/": 0}.toTable

for line in lines("../inputs/07"):
  let words = line.split(" ")
  let path = cwd.join("/")

  if line == "$ cd /":
    cwd = @[""]
  elif line == "$ cd ..":
    cwd = cwd[0..^2]
  elif line.startsWith("$ cd"):
    cwd.add words[2]
  elif line.startsWith("dir"):
    sizes[&"{path}/{words[1]}/"] = 0
  elif line == "$ ls":
    discard
  else:
    sizes[&"{path}/{words[1]}"] = words[0].parseInt

var dirs = initCountTable[string]()

for dir in sizes.keys.toSeq.filter(d => d.endsWith("/")):
  for (file, size) in sizes.pairs:
    if file.startsWith(dir):
      dirs.inc dir, size

let delta = dirs["/"] - 40000000
echo "Part 1: ", dirs.values.toSeq.filter(x => x <= 100000).sum
echo "Part 2: ", dirs.values.toSeq.filter(x => x >= delta).min
