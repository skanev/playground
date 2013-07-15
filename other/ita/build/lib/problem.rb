class Problem
  include Solution

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

  def title
    "Problem #{name}"
  end

  def location
    '%02d/problems/%02d' % components
  end
end
