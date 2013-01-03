class Exercise
  attr_reader :chapter, :number

  def initialize(chapter, number)
    @chapter = chapter.to_i
    @number  = number.to_i
  end

  class << self
    def next
      current_chapter = chapters.last
      last_exercise   = exercises_in_chapter(current_chapter).last
      next_exercise   = last_exercise.to_i + 1

      new current_chapter, next_exercise
    end

    def each
      chapters.each do |chapter|
        exercises_in_chapter(chapter).each do |number|
          yield new chapter, number
        end
      end
    end

    def each_with_test
      each do |exercise|
        next unless exercise.has_test?
        yield exercise
      end
    end

    private

    def chapters
      Dir['*'].grep(/^\d+$/).sort
    end

    def exercises_in_chapter(chapter)
      Dir["%02d/*" % chapter].grep(%r{^\d+/(\d+).scm$}) { $1 }.sort
    end
  end


  def name
    "%d.%02d" % [@chapter, @number]
  end

  def file_path
    "%02d/%02d.scm" % [@chapter, @number]
  end

  def test_path
    "%02d/tests/%02d-tests.scm" % [@chapter, @number]
  end

  def exercise_require_path
    "../%02d.scm" % @number
  end

  def has_test?
    File.exist? test_path
  end
end
