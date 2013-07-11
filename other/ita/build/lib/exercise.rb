class Exercise
  def initialize(chapter, section, number)
    @chapter = chapter.to_i
    @section = section.to_i
    @number  = number.to_i
  end

  def graph?
    @graph ||= SOLUTION_ROOT.join(graph_file).exist?
  end

  def tested?
    @tested ||= SOLUTION_ROOT.join(test_file).exist?
  end

  def code?
    @code ||= SOLUTION_ROOT.join(code_file).exist?
  end

  def components
    [@chapter, @section, @number]
  end

  def name
    "#@chapter.#@section.#@number"
  end

  def markdown_file
    '%02d/%02d/%02d.markdown' % components
  end

  def graph_file
    '%02d/%02d/%02d.dot' % components
  end

  def code_file
    '%02d/%02d/%02d.c' % components
  end

  def test_file
    '%02d/%02d/%02d.test.c' % components
  end

  def test_target_file
    'target/bin/%02d/%02d/%02d' % components
  end

  def run_test
    raise "Exercise #{self} does not have tests" unless tested?

    Runtimes::C.compile_and_run_test test_file, test_target_file
  end
end
