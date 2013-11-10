module Graph
  extend self

  def render(pathname)
    %x[dot -Tgif #{pathname.expand_path.to_s}]
  end
end
