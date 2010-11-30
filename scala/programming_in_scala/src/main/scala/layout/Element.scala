abstract class Element {
  def contents: Array[String]
}

class ArrayElement(val contents: Array[String]) extends Element {
  override def toString = contents.mkString("\n")
  def width = if (contents.isEmpty) 0 else contents(0).length
  def height = contents.size
}
