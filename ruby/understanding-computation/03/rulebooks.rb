require './03'

RULEBOOK_1 = DFARulebook.new([
  FARule.new(1, 'a', 2), FARule.new(1, 'b', 1),
  FARule.new(2, 'a', 2), FARule.new(2, 'b', 3),
  FARule.new(3, 'a', 3), FARule.new(3, 'b', 3)
])

DFA1 = DFADesign.new(1, [3], RULEBOOK_1)

RULEBOOK_2 = NFARulebook.new([
  FARule.new(1, 'a', 1), FARule.new(1, 'b', 1), FARule.new(1, 'b', 2),
  FARule.new(2, 'a', 3), FARule.new(2, 'b', 3),
  FARule.new(3, 'a', 4), FARule.new(3, 'b', 4)
])

NFA1 = NFADesign.new(1, [4], RULEBOOK_2)

RULEBOOK_3 = NFARulebook.new([
  FARule.new(1, nil, 2), FARule.new(1, nil, 4),
  FARule.new(2, 'a', 3),
  FARule.new(3, 'a', 2),
  FARule.new(4, 'a', 5),
  FARule.new(5, 'a', 6),
  FARule.new(6, 'a', 4)
])

NFA2 = NFADesign.new(1, [2, 4], RULEBOOK_3)

PATTERN =
  Repeat.new(
    Choose.new(
      Concatenate.new(Literal.new('a'), Literal.new('b')),
      Literal.new('a')
    )
  )

RULEBOOK_4 = NFARulebook.new([
  FARule.new(1, 'a', 1), FARule.new(1, 'a', 2), FARule.new(1, nil, 2),
  FARule.new(2, 'b', 3),
  FARule.new(3, 'b', 1), FARule.new(3, nil, 2)
])

NFA3 = NFADesign.new(1, [3], RULEBOOK_4)

DFA2 = NFASimulation.new(NFA3).to_dfa_design
