package layout

object Element {
  private class ArrayElement(
    val contents: Array[String]
  ) extends Element

  private class LineElement(line: String) extends Element {
    def contents = Array(line)
  }

  private class UniformElement(
    val fill: Char,
    override val width: Int,
    override val height: Int
  ) extends Element {
    def contents = Array.make(height, fill.toString * width)
  }

  def elem(lines: Array[String]): Element =
    new ArrayElement(lines)
  def elem(line: String): Element =
    new LineElement(line)
  def elem(fill: Char, width: Int, height: Int): Element =
    new UniformElement(fill, width, height)
}

import Element.elem

abstract class Element {
  def contents: Array[String]
  def width = if (contents.isEmpty) 0 else contents(0).length
  def height = contents.size
  override def toString = contents.mkString("\n")

  def above(that: Element): Element =
    elem(this.contents ++ that.contents)

  def beside(that: Element): Element = {
    elem(
      for (
        (line1, line2) <- this.contents zip that.contents
      ) yield line1 + line2
    )
  }
}
