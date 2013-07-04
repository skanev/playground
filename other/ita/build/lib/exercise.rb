class Exercise
  def initialize(chapter, section, number)
    @chapter = chapter.to_i
    @section = section.to_i
    @number  = number.to_i
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
end
