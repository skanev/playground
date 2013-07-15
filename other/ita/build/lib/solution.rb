module Solution
  EXTENSIONS = {c: '.c', python: '.py'}

  def location_path
    SOLUTION_ROOT.join(location)
  end

  def extension_exists?(extension)
    location_path.sub_ext(extension).exist?
  end

  def markdown_path
    location_path.sub_ext '.markdown'
  end

  def graph_path
    location_path.sub_ext '.dot'
  end

  def languages
    @languages ||= EXTENSIONS.
      select { |_, ext| extension_exists? ext }.
      map { |language, _| language }
  end

  def test_languages
    @test_languages ||= EXTENSIONS.
      select { |_, ext| extension_exists? '.test' + ext }.
      map { |language, _| language }
  end

  def run_languages
    @test_languages ||= EXTENSIONS.
      select { |_, ext| extension_exists? '.run' + ext }.
      map { |language, _| language }
  end

  def code?
    not languages.empty?
  end

  def tested?
    not test_languages.empty?
  end

  def graph?
    graph_path.exist?
  end

  def code_path(language)
    location_path.sub_ext EXTENSIONS.fetch(language)
  end

  def test_path(language)
    location_path.sub_ext '.test' + EXTENSIONS.fetch(language)
  end

  def runner_path(language)
    location_path.sub_ext '.run' + EXTENSIONS.fetch(language)
  end

  def run(language)
    Runtimes.for(language).run location
  end

  def run_test(language)
    Runtimes.for(language).run_test location
  end

  def run_and_capture_output(language)
    Runtimes.for(language).run location, true
  end

  def run_tests
    test_languages.each do |language|
      run_test language
    end
  end

  def run_all
    annotate = run_languages.size > 1

    run_languages.each do |language|
      puts "#{language.to_s.upcase}:" if annotate
      run language
      puts if annotate
    end
  end
end
