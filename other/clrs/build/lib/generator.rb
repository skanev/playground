module Generator
  extend self

  def generate
    catalog = Catalog.new SOLUTION_ROOT

    FileUtils.rm_rf 'target/compiled'
    FileUtils.mkdir_p 'target/compiled'
    Dir.chdir 'target/compiled' do
      copy_static_files
      generate_about
      generate_catalog catalog
      generate_solutions catalog
    end
  end

  private

  def copy_static_files
    FileUtils.cp_r '../../build/public/css', 'css'
    FileUtils.cp_r '../../build/public/js', 'js'
    FileUtils.cp_r '../../build/public/img', 'img'
  end

  def generate_solutions(catalog)
    catalog.chapters.each do |chapter|
      chapter.sections.each do |section|
        section.exercises.each do |exercise|
          generate_exercise exercise
          generate_graph exercise if exercise.graph?
        end
      end

      chapter.problems.each do |problem|
        generate_problem problem
      end
    end
  end

  def generate_about
    write_file 'about.html', Renderer.render_about
  end

  def generate_catalog(catalog)
    write_file 'index.html', Renderer.render_catalog(catalog)
  end

  def generate_exercise(exercise)
    filename = '%s/%02d/%02d.html' % exercise.components
    write_file filename, Renderer.render_exercise(exercise)
  end

  def generate_problem(problem)
    filename = '%s/problems/%02d.html' % problem.components
    write_file filename, Renderer.render_problem(problem)
  end

  def generate_graph(exercise)
    filename = '%s/%02d/%02d.png' % exercise.components
    write_file filename, Graph.render(exercise.graph_path)
  end

  def write_file(filename, content)
    path = Pathname(filename).dirname
    FileUtils.mkdir_p path unless path.exist?
    File.open(filename, 'w') { |file| file.write content }
  end
end
