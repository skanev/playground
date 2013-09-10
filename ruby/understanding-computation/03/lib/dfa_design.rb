class DFADesign < Struct.new(:start_state, :accept_states, :rulebook)
  include Dot::Design

  def to_dfa
    DFA.new(start_state, accept_states, rulebook)
  end

  def accepts?(string)
    to_dfa.tap { |dfa| dfa.read_string(string) }.accepting?
  end

  def reverse
    new_accept_states = [start_state]
    new_start_state = Object.new

    start_free_moves = accept_states.map do |state|
      FARule.new(new_start_state, nil, state)
    end

    reversed_rules = rulebook.rules.map do |rule|
      FARule.new(rule.next_state, rule.character, rule.state)
    end

    new_rulebook = NFARulebook.new(start_free_moves + reversed_rules)

    NFADesign.new(new_start_state, new_accept_states, new_rulebook)
  end

  def minimize
    reverse.to_dfa_design.reverse.to_dfa_design
  end
end
