class Exercise
  include Solution

  def initialize(chapter, section, number)
    @chapter = chapter
    @section = section.to_i
    @number  = number.to_i
  end

  def components
    [@chapter, @section, @number]
  end

  def name
    "#{@chapter.short_name}.#@section.#@number"
  end

  def title
    "Exercise #{name}"
  end

  def location
    '%s/%02d/%02d' % components
  end
end
