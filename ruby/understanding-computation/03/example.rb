require_relative '03'
require 'fileutils'

module Chapter03
  extend self

  def generate_example(base_path)
    nfa1 = Pattern.parse('ab(ab)*').to_ast.to_nfa_design
    dfa1 = nfa1.to_dfa_design
    nfa2 = Pattern.parse('a(ba)*b').to_ast.to_nfa_design
    dfa2 = nfa2.to_dfa_design
    dfa1min = dfa1.minimize
    dfa2min = dfa2.minimize

    Dir.chdir(base_path) do
      Dot.draw nfa1, 'nfa1'
      Dot.draw dfa1, 'dfa1'
      Dot.draw nfa2, 'nfa2'
      Dot.draw dfa2, 'dfa2'
      Dot.draw dfa1min, 'dfa1min'
      Dot.draw dfa2min, 'dfa2min'
    end

    FileUtils.cp File.dirname(__FILE__) + '/lib/index.html', base_path.join('index.html')
  end
end

