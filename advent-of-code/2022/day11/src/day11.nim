import strscans, strutils, sequtils, parseutils, sugar, algorithm

type
  Monkey = object
    index: int
    things: seq[int]
    operation: Operation
    divisor: int
    success: int
    failure: int
    inspections: int
  OperandKind = enum Self, Number
  Operator = enum Multiply, Add
  Operation = ref object
    operator: Operator
    case kind: OperandKind
    of Self: discard
    of Number: operand: int

proc parseInput(): seq[Monkey] =
  proc operation(input: string, output: var Operation, start: int): int =
    if input[start + 2..start + 4] == "old":
      output = Operation(kind: Self)
      result = 5
    else:
      output = Operation(kind: Number)
      result = parseutils.parseInt(input[start + 2..^1], output.operand) + 2

    case input[start]
    of '*': output.operator = Multiply
    of '+': output.operator = Add
    else: raise

  proc numbers(input: string, output: var seq[int], start: int): int =
    var parts: string
    result = parseWhile(input[start..^1], parts, Digits + {',', ' '})
    output = input[start..<start+result].split(", ").map(parseInt)

  const format = """
Monkey $i:
  Starting items: ${numbers}
  Operation: new = old ${operation}
  Test: divisible by $i
    If true: throw to monkey $i
    If false: throw to monkey $i
"""

  collect:
    for chunk in readFile("../inputs/11").split("\n\n"):
      var monkey: Monkey
      discard scanf(chunk, format, monkey.index, monkey.things, monkey.operation, monkey.divisor, monkey.success, monkey.failure)
      monkey

proc apply(op: Operation, number: int): int =
  case op.kind
    of Self: result = number
    of Number: result = op.operand

  case op.operator
    of Multiply: result *= number
    of Add: result += number

proc solve(turns: int, fn: (int) -> int): int =
  var monkeys = parseInput()

  for _ in 1..turns:
    for monkey in monkeys.mitems:
      for item in monkey.things:
        let item = monkey.operation.apply(item).fn

        if (item mod monkey.divisor) == 0:
          monkeys[monkey.success].things.add(item)
        else:
          monkeys[monkey.failure].things.add(item)

        inc monkey.inspections

      monkey.things.setLen(0)

  return monkeys.mapIt(-it.inspections).sorted[0..1].foldl(a * b)

let m = parseInput().mapIt(it.divisor).foldl(a * b)

echo solve(20, x => x div 3)
echo solve(10000, x => x mod m)
