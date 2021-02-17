module Graph
  extend self

  def render_png(pathname)
    %x[dot -Tpng -Gdpi=144 #{pathname.expand_path.to_s}]
  end

  def render_svg(pathname)
    %x[dot -Tsvg #{pathname.expand_path.to_s}]
  end

  def list_drawings(pathname)
    run_draw(pathname, 'list').lines.map { |line| line.split(/\s+/, 4) }
  end

  def render_drawing(pathname, number)
    run_draw pathname, 'draw', number
  end

  private

  def run_draw(pathname, *args)
    %x[PYTHONPATH="#{EXT_ROOT}" PYTHONDONTWRITEBYTECODE=x python3 #{pathname} #{args.join(' ')}]
  end
end
