module Graph
  extend self

  def render_png(pathname)
    %x[dot -Tpng -Gdpi=144 #{pathname.expand_path.to_s}]
  end

  def render_svg(pathname)
    %x[dot -Tsvg #{pathname.expand_path.to_s}]
  end
end
