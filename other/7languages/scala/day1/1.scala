object Player extends Enumeration {
  val X = Value("X")
  val O = Value("O")

  def fromChar(char : Char) : Player.Value = {
    char match {
      case 'X' => X
      case 'O' => O
      case ' ' => null
      case _   => throw new AssertionError("Illegal character " + char)
    }
  }
}

import Player.X
import Player.O

object Board {
  val rows = List((0, 1, 2), (3, 4, 5), (6, 7, 8))
  val columns = List((0, 3, 6), (1, 4, 7), (2, 5, 8))
  val diagonals = List((0, 4, 8), (2, 4, 6))
  val lanes = rows ::: columns ::: diagonals
}

class Board(marks : List[Player.Value]) {
  assert(marks.length == 9)

  def this(boardAsString : String) {
    this(boardAsString.map(Player.fromChar).toList)
    assert(boardAsString.matches("^[XO ]{9}$"))
  }

  def winner() : Player.Value = {
    val markedLanes = Board.lanes.map(marksOnLane)
    markedLanes.foreach { lane =>
      lane match {
        case (X, X, X) => return X
        case (O, O, O) => return O
        case _ =>
      }
    }
    return null
  }

  def marksOnLane(lane : (Int, Int, Int)) : (Player.Value, Player.Value, Player.Value) = {
    (marks(lane._1), marks(lane._2), marks(lane._3))
  }
}

println(new Board("XXXO  X  ").winner) // X
println(new Board("X  O  X  ").winner) // null
println(new Board("XO OO XO ").winner) // O
println(new Board("XO OX XOX").winner) // X
