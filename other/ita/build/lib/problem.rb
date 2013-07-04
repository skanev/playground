class Problem
  def initialize(chapter, number)
    @chapter = chapter.to_i
    @number  = number.to_i
  end

  def components
    [@chapter, @number]
  end

  def name
    "#@chapter.#@number"
  end

  def markdown_file
    '%02d/problems/%02d.markdown' % components
  end
end

