require 'set'
require 'treetop'

$:.unshift File.dirname(__FILE__) + '/lib'

autoload :FARule, 'fa_rule'
autoload :DFARulebook, 'dfa_rulebook'
autoload :DFA, 'dfa'
autoload :DFADesign, 'dfa_design'
autoload :NFARulebook, 'nfa_rulebook'
autoload :NFA, 'nfa'
autoload :NFADesign, 'nfa_design'
autoload :NFASimulation, 'nfa_simulation'
autoload :Dot, 'dot'

autoload :Pattern, 'pattern'
autoload :Empty, 'pattern'
autoload :Literal, 'pattern'
autoload :Concatenate, 'pattern'
autoload :Choose, 'pattern'
autoload :Repeat, 'pattern'

Treetop.load(File.dirname(__FILE__) + '/lib/grammar')
