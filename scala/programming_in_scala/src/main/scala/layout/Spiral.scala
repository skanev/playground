package layout

import Element.elem

object Spiral {
  val space = elem(" ")
  val corner = elem("+")

  def spiral(edges: Int): Element = spiral(edges, 0)
  def spiral(edges: Int, direction: Int): Element = {
    if (edges == 1)
      elem("+")
    else {
      val innerSpiral = spiral(edges - 1, (direction + 3) % 4)
      def verticalBar = elem('|', 1, innerSpiral.height)
      def horizontalBar = elem('-', innerSpiral.width, 1)
      direction match {
        case 0 => (corner beside horizontalBar) above (innerSpiral beside space)
        case 1 => (innerSpiral above space) beside (corner above verticalBar)
        case 2 => (space beside innerSpiral) above (horizontalBar beside corner)
        case _ => (verticalBar above corner) beside (space above innerSpiral)
      }
    }
  }

  def main(args: Array[String]) {
    val sides = args(0).toInt
    println(spiral(sides))
  }
}
