module SimpleParser
  START_RULE = PDARule.new(1, nil, 2, '$', %w[S $])
  SYMBOL_RULES = [
    # <statement> ::= <while> | <assign>
    PDARule.new(2, nil, 2, 'S', %w[W]),
    PDARule.new(2, nil, 2, 'S', %w[A]),

    # <while> ::= 'w' '(' <expression> ')' '{' <statement> '}'
    PDARule.new(2, nil, 2, 'W', %w[w ( E ) { S }]),

    # <assign> ::= 'v' '=' <expression>
    PDARule.new(2, nil, 2, 'A', %w[v = E]),

    # <expression> ::= <less-than>
    PDARule.new(2, nil, 2, 'E', %w[L]),

    # <less-than> ::= <multiply> '<' <less-than> | <multiply>
    PDARule.new(2, nil, 2, 'L', %w[M < L]),
    PDARule.new(2, nil, 2, 'L', %w[M]),

    # <multiply> ::= <term> '*' <multiply> | <term>
    PDARule.new(2, nil, 2, 'M', %w[T * M]),
    PDARule.new(2, nil, 2, 'M', %w[T]),

    # <term> ::= 'n' | 'v'
    PDARule.new(2, nil, 2, 'T', %w[n]),
    PDARule.new(2, nil, 2, 'T', %w[v]),
  ]
  TOKEN_RULES = LexicalAnalyzer::GRAMMAR.map do |rule|
    PDARule.new(2, rule[:token], 2, rule[:token], [])
  end
  STOP_RULE = PDARule.new(2, nil, 3, '$', %w[$])

  RULES = [START_RULE, STOP_RULE] + SYMBOL_RULES + TOKEN_RULES
  RULEBOOK = NPDARulebook.new(RULES)
  DESIGN = NPDADesign.new(1, '$', [3], RULEBOOK)

  def self.accepts?(string)
    token_string = LexicalAnalyzer.new(string).analyze.join
    puts token_string
    DESIGN.accepts?(token_string)
  end
end
