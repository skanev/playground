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

  def above(that: Element): Element = {
    val adjustedThis = this widen that.width
    val adjustedThat = that widen this.width
    elem(adjustedThis.contents ++ adjustedThat.contents)
  }

  def beside(that: Element): Element = {
    val adjustedThis = this heighten that.height
    val adjustedThat = that heighten this.height
    elem(
      for (
        (line1, line2) <- adjustedThis.contents zip adjustedThat.contents
      ) yield line1 + line2
    )
  }

  def widen(w: Int): Element = {
    if (w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }
  }

  def heighten(h: Int): Element = {
    if (h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      val bottom = elem(' ', width, h - height - top.height)
      top above this above bottom
    }
  }
}
