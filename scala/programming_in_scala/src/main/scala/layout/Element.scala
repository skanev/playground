package layout

object Element {
  def elem(lines: Array[String]) =
    new ArrayElement(lines)
  def elem(line: String) =
    new LineElement(line)
  def elem(fill: Char, width: Int, height: Int) =
    new UniformElement(fill, width, height)
}

abstract class Element {
  def contents: Array[String]
  def width = if (contents.isEmpty) 0 else contents(0).length
  def height = contents.size
  override def toString = contents.mkString("\n")
}

class ArrayElement(val contents: Array[String]) extends Element

class LineElement(line: String) extends Element {
  def contents = Array(line)
}

class UniformElement(
  val fill: Char,
  override val width: Int,
  override val height: Int
) extends Element {
  def contents = Array.make(height, fill.toString * width)
}
