class Catalog
  Chapter = Struct.new(:number, :sections, :problems)
  Section = Struct.new(:number, :exercises)
  Position = Struct.new(:before, :after)

  class << self
    def locate
      new Pathname(__FILE__).join('../../..')
    end
  end

  def initialize(root_dir)
    @root = root_dir
  end

  def chapters
    @chapters ||= find_chapters
  end

  def solutions
    load_solutions_and_positions
    @solutions
  end

  def positions
    load_solutions_and_positions
    @positions
  end

  def previous(solution)
    positions[solution.name].before
  end

  def next(solution)
    positions[solution.name].after
  end

  private

  def load_solutions_and_positions
    return if @solutions && @positions

    @solutions = {}
    @positions = {}

    solutions = chapters.flat_map { |chapter| chapter.sections.flat_map(&:exercises) + chapter.problems }

    before = nil

    solutions.each do |solution|
      name = solution.name
      @positions[before.name].after = solution if before
      @solutions[name] = solution
      @positions[name] = Position.new before, nil
      before = solution
    end

    @solutions
  end

  def find_chapters
    Dir.chdir @root do
      dirs = glob('.', '[0-9][0-9]').to_a + glob('.', '[ABCD]').to_a

      dirs.map do |dir|
        number   = ChapterNumber.new dir
        sections = find_sections number
        problems = find_problems number

        Chapter.new number, sections, problems
      end
    end
  end

  def find_sections(chapter)
    glob chapter, '[0-9][0-9]' do |number|
      exercises = find_exercises chapter, number
      Section.new number.to_i, exercises
    end
  end

  def find_exercises(chapter, section)
    glob "#{chapter}/#{section}", '[0-9][0-9]', 'markdown' do |number|
      Exercise.new chapter, section, number
    end
  end

  def find_problems(chapter)
    dir = Pathname("#{chapter}/problems")
    return [] unless dir.exist?

    glob dir, '[0-9][0-9]', 'markdown' do |number|
      Problem.new chapter, number
    end
  end

  def glob(dir, pattern, extension = nil, &block)
    pattern = "#{pattern}.#{extension}" if extension
    matches = Dir.chdir(dir) { Dir.glob(pattern) }
    matches = matches.map { |name| name.gsub(/\.#{extension}$/, '') } if extension
    matches.sort.map(&block)
  end
end
