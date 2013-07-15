class Exercise
  include Solution

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

  def title
    "Exercise #{name}"
  end

  def location
    '%02d/%02d/%02d' % components
  end
end
