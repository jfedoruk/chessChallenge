package chessChallenge

import chessChallenge.ChessPiece.{Bishop, Knight, Queen, Rook}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Test suite for Chess Challenge app
  */
class ChessChallengeTestSuite extends FlatSpec with Matchers with Solver {

  "A King case class" must "return correct position" in {
    val king = new ChessPiece.Knight(Position(1, 1))
    assert(king.pos.row === 1)
    assert(king.pos.col === 1)
  }

  "A Queen case class" must "return correct position" in {
    val queen = new ChessPiece.Queen(Position(2, 3))
    assert(queen.pos.row === 2)
    assert(queen.pos.col === 3)
  }

  "A chess piece factory" must "return error when wrong class is provided" in {
    assertThrows[RuntimeException] {
      val piece = ChessPiece("WRONG", Position(1, 1))
    }
  }

  "A chess piece factory" must "return a correct class" in {
    assert(ChessPiece("K", Position(1,1)) == ChessPiece.King(Position(1,1)))
    val queen = ChessPiece("Q", Position(2,2))
    val knight = ChessPiece("N", Position(2,4))
    val bishop = ChessPiece("B", Position(3,3))
    val rook = ChessPiece("R", Position(3,3))

    queen shouldBe a [Queen]
    knight shouldBe a [Knight]
    bishop shouldBe a [Bishop]
    rook shouldBe a [Rook]
  }

  "A chess board class" must "return a chess board class type" in {
     val cb = new ChessBoard(1, 1)
     cb shouldBe a [ChessBoard]
  }

  "Solver " must "return a correct number of solutions" in {
    object solution extends Solver
    assert(0 == solution.solve(List(), new ChessBoard(2, 2))._1)
    assert(0 == solution.solve(List("K", "K"), new ChessBoard(2, 2))._1)
    assert(2 == solution.solve(List("R", "R"), new ChessBoard(2, 2))._1)
    assert(4 == solution.solve(List("K", "K", "R"), new ChessBoard(3, 3))._1)
    assert(6 == solution.solve(List("N", "N"), new ChessBoard(2, 2))._1)
    assert(4 == solution.solve(List("B", "B"), new ChessBoard(2, 2))._1)
    assert(0 == solution.solve(List("Q", "Q"), new ChessBoard(2, 2))._1)
    assert(8 == solution.solve(List("Q", "Q"), new ChessBoard(3, 3))._1)
    assert(6 == solution.solve(List("R", "R", "N"), new ChessBoard(3, 3))._1)
    assert(8 == solution.solve(List("R", "R", "N", "N", "N", "N"), new ChessBoard(4, 4))._1)
  }

  "Solver " must "return a correct number of solutions using 2nd algorithm" in {
    object solution extends Solver
    assert(0 == solution.solve2(List("K", "K"), new ChessBoard(2, 2))._1)
    assert(2 == solution.solve2(List("R", "R"), new ChessBoard(2, 2))._1)
    assert(4 == solution.solve2(List("K", "K", "R"), new ChessBoard(3, 3))._1)
    assert(6 == solution.solve2(List("N", "N"), new ChessBoard(2, 2))._1)
    assert(4 == solution.solve2(List("B", "B"), new ChessBoard(2, 2))._1)
    assert(0 == solution.solve2(List("Q", "Q"), new ChessBoard(2, 2))._1)
    assert(8 == solution.solve2(List("Q", "Q"), new ChessBoard(3, 3))._1)
    assert(6 == solution.solve2(List("R", "R", "N"), new ChessBoard(3, 3))._1)
    assert(8 == solution.solve2(List("R", "R", "N", "N", "N", "N"), new ChessBoard(4, 4))._1)
  }

  "Solver " must "give a correct number of solutions for main challenge" in {
    object solution extends Solver
    assert(3063828 == solution.solve(List("K", "K", "Q", "Q", "B", "B", "N"), new ChessBoard(7, 7))._1)
  }

  "New algorithm " should "be faster than old one" in {
    object solution extends Solver

    var totalTimeOld = 0L

    for (i <- 0 until 10)
    {
      val t0 = System.nanoTime()
      solution.solve(List("K", "K", "Q", "Q", "B", "B", "N"), new ChessBoard(6, 6))
      totalTimeOld += (System.nanoTime() - t0)
    }

    println("Old method:" + totalTimeOld / 10)

    var totalTimeNew = 0L

    for (i <- 0 until 10)
    {
      val t0 = System.nanoTime()
      solution.solve2(List("K", "K", "Q", "Q", "B", "B", "N"), new ChessBoard(6, 6))
      totalTimeNew += (System.nanoTime() - t0)
    }

    println("New method:" + totalTimeNew / 10)

    assert(totalTimeNew < totalTimeOld)
  }

}
