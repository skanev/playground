module Generator
  extend self

  def generate
    catalog = Catalog.new SOLUTION_ROOT

    FileUtils.rm_rf 'target/compiled'
    FileUtils.mkdir_p 'target/compiled'
    Dir.chdir 'target/compiled' do
      copy_static_files
      generate_css
      generate_catalog catalog
      generate_solutions catalog
    end
  end

  private

  def copy_static_files
    FileUtils.cp_r '../../build/public/css', 'css'
    FileUtils.cp_r '../../build/public/img', 'img'
  end

  def generate_css
    write_file 'css/clrs.css', Renderer.render_css
  end

  def generate_solutions(catalog)
    catalog.chapters.each do |chapter|
      next unless chapter.number.name == '04'
      chapter.sections.each do |section|
        section.exercises.each do |exercise|
          generate_exercise exercise, catalog
          generate_graph exercise if exercise.graph?
          generate_drawings exercise if exercise.drawings?
          generate_additional_graphs exercise if exercise.additional_graphs?
        end
      end

      chapter.problems.each do |problem|
        generate_problem problem, catalog
        generate_graph problem if problem.graph?
      end
    end
  end

  def generate_catalog(catalog)
    write_file 'index.html', Renderer.render_catalog(catalog)
  end

  def generate_exercise(exercise, catalog)
    write_file "#{exercise.location}.html", Renderer.render_exercise(exercise, catalog)
  end

  def generate_problem(problem, catalog)
    write_file "#{problem.location}.html", Renderer.render_problem(problem, catalog)
  end

  def generate_additional_graphs(solution)
    solution.additional_graph_paths.each do |path|
      write_file Pathname(solution.location).dirname.join(path.sub_ext('.png').basename).to_s, Graph.render_png(path)
      write_file Pathname(solution.location).dirname.join(path.sub_ext('.svg').basename).to_s, Graph.render_svg(path)
    end
  end

  def generate_drawings(solution)
    Graph.list_drawings(solution.draw_path).each do |number, file, display|
      write_file file, Graph.render_drawing(solution.draw_path, number)
    end
  end

  def generate_graph(solution)
    write_file "#{solution.location}.png", Graph.render_png(solution.graph_path)
    write_file "#{solution.location}.svg", Graph.render_svg(solution.graph_path)
  end

  def write_file(filename, content)
    path = Pathname(filename).dirname
    FileUtils.mkdir_p path unless path.exist?
    File.open(filename, 'w') { |file| file.write content }
  end
end
