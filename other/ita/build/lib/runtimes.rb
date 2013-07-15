module Runtimes
  extend self

  def for(language)
    case language
      when :c      then Runtimes::C
      when :python then Runtimes::Python
      else              raise "Unknown language #{language}"
    end
  end
end
