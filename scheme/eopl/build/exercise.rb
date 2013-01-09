class Exercise
  attr_reader :chapter, :number

  def initialize(chapter, number)
    @chapter = case chapter
      when Integer then chapter
      when /^\d+$/ then chapter.to_i
      when /^B$/ then 'B'
      else raise "Invalid chapter: #{chapter}"
    end
    @number  = number.to_i
  end

  class << self
    def next
      current_chapter = chapters(false).last
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

    def chapters(include_appendices = true)
      chapters = Dir['*'].grep(/^(\d+|B)$/)
      chapters -= %w[A B] unless include_appendices
      chapters.sort
    end

    def exercises_in_chapter(chapter)
      Dir["#{chapter}/*"].grep(%r{^(?:\d+|B)/(\d+).scm$}) { $1 }.sort
    end
  end


  def name
    "%s.%02d" % [@chapter, @number]
  end

  def file_path
    "%s/%02d.scm" % [formatted_chapter, @number]
  end

  def test_path
    "%s/tests/%02d-tests.scm" % [formatted_chapter, @number]
  end

  def exercise_require_path
    "../%02d.scm" % @number
  end

  def has_test?
    File.exist? test_path
  end

  private

  def formatted_chapter
    case @chapter
      when Integer then "%02d" % @chapter
      else @chapter
    end
  end
end
