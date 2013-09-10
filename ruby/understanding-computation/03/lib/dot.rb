module Dot
  module Design
    def to_dot
      states = rulebook.rules.flat_map(&:states).uniq

      dot = 'digraph NFA {'

      states.each do |state|
        style = if start_state == state
          ' [style=filled]'
        elsif accept_states.include?(state)
          ' [peripheries=2]'
        else
          ''
        end

        dot << "#{Dot.repr(state)}#{style};"
      end

      rulebook.rules.each do |rule|
        dot << rule.to_dot
        dot << ';'
      end
      dot << '}'
    end
  end

  module Rule
    def states
      [state, next_state]
    end

    def to_dot
      edge_style = if character
        "label=#{character}"
      else
        'style=dashed'
      end

      "#{state_repr} -> #{next_state_repr} [#{edge_style}]"
    end

    def state_repr
      repr state
    end

    def next_state_repr
      repr next_state
    end

    private

    def repr(state)
      Dot.repr(state)
    end
  end

  def self.draw(design, file_base, open=false)
    File.open("#{file_base}.dot", 'w') do |file|
      file.write design.to_dot
    end

    system "dot -Tgif #{file_base}.dot > #{file_base}.gif"
    system "open #{file_base}.gif" if open
  end

  def self.repr(state)
    case state
    when Integer then state.to_s
    when Set[], nil then 'None'
    else gensym(state)
    end
  end

  def self.reset_gensym
    @states = {}
    @current = 'A'
  end

  def self.shorten_set(set_of_sets)
    @sets ||= {}
    @current_set_name ||= 'AA'

    if @sets.has_key? set_of_sets
      @sets[set_of_sets]
    else
      @sets[set_of_sets] = @current_set_name
      @current_set_name = @current_set_name.succ
      @sets[set_of_sets]
    end
  end

  def self.gensym(state)
    if state.is_a?(Set) and state.none? { |i| i.is_a? Set }
      return '"' + state.to_a.map { |i| gensym(i) }.sort.join('/') + '"'
    end

    key = case state
      when Set then state
      else state.object_id
    end

    @states ||= {}
    @current ||= 'A'

    if @states[key]
      @states[key]
    else
      @states[key] = @current
      @current = @current.succ
      @states[key]
    end
  end
end
