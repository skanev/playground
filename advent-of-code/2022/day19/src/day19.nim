import strutils, sequtils, sets, math

type
  Blueprint = seq[int]
  State = array[9, int]

var blueprints = newSeq[Blueprint]()
for line in lines("../inputs/19"):
  blueprints.add line.replace(":", "").split(' ').filterIt(it.all(isDigit)).map(parseInt)

proc `+`(a, b: State): State =
  for i in 0..8:
    result[i] = a[i] + b[i]

proc solve(blueprint: Blueprint, steps: int): int =
  var best = 0

  let
    a = blueprint[1]
    b = blueprint[2]
    c = blueprint[3]
    d = blueprint[4]
    e = blueprint[5]
    f = blueprint[6]

  var seen: HashSet[State]

  let max1 = max([a, b, c, e])
  let max2 = d
  let max3 = f

  proc dfs(state: State) =
    let t = state[0]

    if t == 0:
      best = max(best, state[8])
      return

    var state = state

    state[1] = min(state[1], max1)
    state[2] = min(state[2], max2)
    state[3] = min(state[3], max3)

    state[5] = min(state[5], t * max1 - state[1] * (t - 1))
    state[6] = min(state[6], t * max2 - state[2] * (t - 1))
    state[7] = min(state[7], t * max3 - state[3] * (t - 1))

    if state in seen:
      return

    seen.incl state

    let next = state + [-1, 0, 0, 0, 0, state[1], state[2], state[3], state[4]]

    dfs next

    if state[5] >= a: dfs next + [0, 1, 0, 0, 0, -a, 0, 0, 0]
    if state[5] >= b: dfs next + [0, 0, 1, 0, 0, -b, 0, 0, 0]
    if state[5] >= c and state[6] >= d: dfs next + [0, 0, 0, 1, 0, -c, -d, 0, 0]
    if state[5] >= e and state[7] >= f: dfs next + [0, 0, 0, 0, 1, -e, 0, -f, 0]

  dfs [steps, 1, 0, 0, 0, 0, 0, 0, 0]

  return best

echo blueprints.mapIt(solve(it, 24) * it[0]).sum
echo blueprints[0..2].mapIt(solve(it, 32)).foldl(a * b)
