class FARule < Struct.new(:state, :character, :next_state)
  include Dot::Rule

  def applies_to?(state, character)
    self.state == state and
      self.character == character
  end

  def follow
    next_state
  end

  def inspect
    "#<FARule #{state.inspect} --#{character}--> #{next_state.inspect}>"
  end
end
