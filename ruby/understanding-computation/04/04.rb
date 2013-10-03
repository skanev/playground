require 'set'

$:.unshift File.dirname(__FILE__) + '/lib'

autoload :Stack, 'stack'
autoload :PDARule, 'pda_rule'
autoload :PDAConfiguration, 'pda_configuration'

autoload :DPDA, 'dpda'
autoload :DPDADesign, 'dpda_design'
autoload :DPDARulebook, 'dpda_rulebook'

autoload :NPDA, 'npda'
autoload :NPDADesign, 'npda_design'
autoload :NPDARulebook, 'npda_rulebook'

autoload :LexicalAnalyzer, 'lexical_analyzer'
autoload :SimpleParser, 'simple_parser'
