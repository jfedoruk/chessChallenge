package chessChallenge

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
  }

  "A chess board class" must "return a chess board class type" in {
     val cb = new ChessBoard(1, 1)
     cb shouldBe a [ChessBoard]
  }

  "Solver " must "return a number of solutions" in {
    object solution extends Solver
    assert(4 == solution.solve(List("K", "K"), new ChessBoard(3, 3)))
  }
}
