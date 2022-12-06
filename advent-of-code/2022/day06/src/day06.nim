import sets

proc solve(size: int): int =
  let chars = readFile("../inputs/06")
  for i in 0 ..< chars.len - size:
    if chars[i..<i+size].toHashSet.len == size:
      return i + size

echo "Part 1: ", solve(4)
echo "Part 2: ", solve(14)
