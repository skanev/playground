class NFADesign < Struct.new(:start_state, :accept_states, :rulebook)
  include Dot::Design

  def accepts?(string)
    to_nfa.tap { |nfa| nfa.read_string(string) }.accepting?
  end

  def to_nfa(current_states = Set[start_state])
    NFA.new(current_states, accept_states, rulebook)
  end

  def to_dfa_design
    NFASimulation.new(self).to_dfa_design
  end
end
