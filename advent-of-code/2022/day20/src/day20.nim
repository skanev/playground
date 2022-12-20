import strutils, lists

proc properMod(a, b: int): int = (a mod b + b) mod b

proc advance(node: var DoublyLinkedNode[int], count: int) =
  for _ in 0..<count:
    node = node.next

proc solve2(mixes: int, key: int): int =
  var ring = initDoublyLinkedRing[int]()
  var nodes = newSeq[DoublyLinkedNode[int]]()

  for line in lines("../inputs/20"):
    let number = parseInt(line) * key
    var node = newDoublyLinkedNode(number)
    ring.add node
    nodes.add node

  for _ in 1..mixes:
    for node in nodes.mitems:
      var previous = node.prev
      ring.remove node

      previous.advance properMod(node.value, nodes.len - 1)

      previous.next.prev = node
      node.prev = previous
      node.next = previous.next
      previous.next = node

  var node = ring.find(0)

  for _ in 1..3:
    node.advance 1000
    result += node.value

echo solve2(1, 1)
echo solve2(10, 811589153)
