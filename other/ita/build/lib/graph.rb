module Graph
  extend self

  def render(pathname)
    %x[dot -Tgif #{SOLUTION_ROOT.join(pathname).expand_path.to_s}]
  end
end
