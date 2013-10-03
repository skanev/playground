class LexicalAnalyzer < Struct.new(:string)
  GRAMMAR = [
    {token: 'i', pattern: /if/         },
    {token: 'e', pattern: /else/       },
    {token: 'w', pattern: /while/      },
    {token: 'd', pattern: /do-nothing/ },
    {token: '(', pattern: /\(/         },
    {token: ')', pattern: /\)/         },
    {token: '{', pattern: /\{/         },
    {token: '}', pattern: /\}/         },
    {token: ';', pattern: /;/          },
    {token: '=', pattern: /=/          },
    {token: '+', pattern: /\+/         },
    {token: '*', pattern: /\*/         },
    {token: '<', pattern: /</          },
    {token: 'n', pattern: /[0-9]+/     },
    {token: 'b', pattern: /true|false/ },
    {token: 'v', pattern: /[a-z]+/     },
  ]

  def analyze
    [].tap do |tokens|
      while more_tokens?
        tokens.push(next_token)
      end
    end
  end

  def more_tokens?
    !string.empty?
  end

  def next_token
    rule, match = rule_matching(string)
    self.string = string_after(match)
    rule[:token]
  end

  def rule_matching(string)
    matches = GRAMMAR.map { |rule| match_at_beginning(rule[:pattern], string) }
    rules_with_matches = GRAMMAR.zip(matches).reject { |rule, match| match.nil? }
    rule_with_longest_match(rules_with_matches)
  end

  def match_at_beginning(pattern, string)
    /\A#{pattern}/.match(string)
  end

  def rule_with_longest_match(rules_with_matches)
    rules_with_matches.max_by { |rule, match| match.to_s.length }
  end

  def string_after(match)
    match.post_match.lstrip
  end
end
