module Renderer
  extend self

  def render_catalog(catalog)
    context = make_context catalog: catalog

    render_view 'layout', context do
      render_view 'catalog', context
    end
  end

  def render_exercise(exercise)
    exercise_markdown = File.read exercise.markdown_path
    context           = make_context exercise: exercise, base: '../../'

    render_view 'layout', context do
      render_view 'exercise', context do
        process exercise_markdown
      end
    end
  end

  def render_problem(problem)
    problem_markdown = File.read problem.markdown_path
    context          = make_context problem: problem, base: '../../'

    render_view 'layout', context do
      render_view 'problem', context do
        process problem_markdown
      end
    end
  end

  def render_code(path, language)
    code = File.read path
    CodeRay.scan(code, language).div
  end

  def render_about
    render_view 'layout', make_context do
      render_view 'about', make_context
    end
  end

  private

  def render_view(name, context, &block)
    filename = VIEWS_ROOT.join("#{name}.erb").to_s
    Tilt::ERBTemplate.new(filename).render(context, &block)
  end

  def make_context(hash = {})
    Object.new.tap do |context|
      hash.each do |key, value|
        context.instance_variable_set :"@#{key}", value
      end
    end
  end

  def markdown
    @markdown ||= Redcarpet::Markdown.new Markdown, tables: true, no_intra_emphasis: true
  end

  def process(markdown_code)
    markdown_code = markdown_code.gsub(/exercise\s+([A-D]|\d+).(\d+)[-.](\d+)/i) { |text| "[#{text}](/%s/%02d/%02d.html)" % [ChapterNumber.new($1), $2, $3] }
    markdown_code = markdown_code.gsub(/problem\s+([A-D]|\d+).(\d+)/i) { |text| "[#{text}](/%s/problems/%02d.html)" % [ChapterNumber.new($1), $2] }
    markdown.render markdown_code
  end

  class Markdown < Redcarpet::Render::HTML
    def postprocess(html)
      doc = Nokogiri::HTML(html)
      doc.search('table').each do |node|
        node[:class] = 'table table-bordered table-striped table-compact'
      end
      doc.search('body').first.inner_html
    end
  end
end
