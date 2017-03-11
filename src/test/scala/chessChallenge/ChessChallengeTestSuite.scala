package chessChallenge

import org.scalatest.FlatSpec

/**
  * Test suite for Chess Challenge app
  */
class ChessChallengeTestSuite extends FlatSpec with Solver {

  "A King case class" must "return correct position" in {
    val king = new King(Position(1, 1))
    assert(king.pos.row === 1)
    assert(king.pos.col === 1)
  }

  "A Queen case class" must "return correct position" in {
    val queen = new Queen(Position(2, 3))
    assert(queen.pos.row === 2)
    assert(queen.pos.col === 3)
  }
}
