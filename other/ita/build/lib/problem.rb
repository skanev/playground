class Problem
  include Solution

  def initialize(chapter, number)
    @chapter = chapter
    @number  = number.to_i
  end

  def components
    [@chapter, @number]
  end

  def name
    "#{@chapter.short_name}.#@number"
  end

  def title
    "Problem #{name}"
  end

  def location
    '%s/problems/%02d' % components
  end
end
