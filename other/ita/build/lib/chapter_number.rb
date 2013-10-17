class ChapterNumber
  def initialize(name)
    @name = case name
      when /^(\d+)$/ then name.to_i
      when /^[ABCD]$/ then name
      else raise "Invalid chapter name: #{name}"
    end
  end

  def name
    case @name
      when Integer then '%02d' % @name
      when String then @name
      else raise '???'
    end
  end

  def short_name
    @name.to_s
  end

  def inspect
    "#<Chapter:#{name}>"
  end

  alias to_s name
  alias to_str to_s
end
