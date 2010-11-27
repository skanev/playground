object Player extends Enumeration {
  val X = Value("X")
  val O = Value("O")

  def other(value : Value) = if (value == X) { O } else { X }
  def asChar(value : Value) : Char = {
    value match {
      case X => 'X'
      case O => 'O'
      case null => ' '
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

class Board {
  var marks : Array[Player.Value] = Array(null, null, null, null, null, null, null, null, null)

  def apply(square : Int) = marks(square)
  def marksOnLane(lane : (Int, Int, Int)) = (marks(lane._1), marks(lane._2), marks(lane._3))
  def taken(square : Int) = marks(square) != null
  def isFull = marks.forall(_ != null)
  def hasWinner = winner != null

  def placeMark(player : Player.Value, square : Int) {
    marks(square) = player
  }

  def winner : Player.Value = {
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
}

object GridRenderer {
  def render(grid : Seq[Char]) = new GridRenderer(grid).render
}

class GridRenderer(grid : Seq[Char]) {
  def render {
    println(separator)
    for (row <- 0 to 2) {
      println(rowLine(row))
      println(separator)
    }
  }

  def separator = "+---+---+---+"
  def rowLine(row : Int) = "| %c | %c | %c |".format(at(row, 0), at(row, 1), at(row, 2))
  def at(row : Int, column : Int) = grid(row * 3 + column)
}

class Game {
  var currentPlayer = X
  val board = new Board

  def play {
    renderSquareNumbers

    while (inProgress) {
      playOn(readPosition)
      renderBoard
      switchPlayer
    }

    showOutcome
  }

  def inProgress = !(board.hasWinner || board.isFull)
  def renderBoard = GridRenderer render board.marks.map(Player.asChar)
  def renderSquareNumbers = GridRenderer render '1'.to('9')
  def playOn(square : Int) = board.placeMark(currentPlayer, square)
  def switchPlayer = this.currentPlayer = Player.other(currentPlayer)

  def showOutcome {
    if (board.winner != null) {
      println("The winner is player " + board.winner + ".")
    } else {
      println("The game was a tie.")
    }
  }

  def readPosition : Int = {
    while (true) {
      print("Player " + currentPlayer + "'s turn. Pick square (1-9): ")
      try {
        val input = Console.readInt - 1
        if (!0.to(8).contains(input)) {
          println("Specify the square with a number from 1 to 9")
        } else if (board.taken(input)) {
          println("Square is taken. Try another one")
        } else {
          return input
        }
      } catch {
        case e: NumberFormatException => println("Please enter an integer between 1 and 9.")
      }
    }

    throw new AssertionError("Unreachable code :P")
  }
}

new Game().play
