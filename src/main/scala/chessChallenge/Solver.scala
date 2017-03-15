package chessChallenge

import scala.annotation.tailrec

/**
  * This trait implements solver for Chess Challenge
  */
trait Solver {

  /**
    * Holds list of solutions
    */
  val solution: Set[List[ChessPiece]] = Set(List())

  /**
    * This functions place all the pieces on the given chess board.
    *
    * Each piece will be placed only if it does not threat other
    * already placed pieces. Also the piece cannot threat other
    * pieces after taking the position on the board.
    *
    * Result of the operation is sorted to eliminate duplications.
    *
    * Note: this is a recursive function
    *
    * @param pieces list of pieces to be placed
    * @param cb chessboard where pieces will be placed
    * @return set of solutions for given input params
    */
  def placePieces(pieces: List[String], cb: ChessBoard) : Set[List[ChessPiece]] = pieces match {
    case Nil => Set(List())
    case head :: tail =>
      for {
        placed <- placePieces(tail, cb)
        row <- 0 until cb.M
        col <- 0 until cb.N
        piece = ChessPiece(head, Position(row, col))
        if placed.forall(x => !x.isThreat(piece)) && placed.forall(x => !piece.isThreat(x))
      } yield (piece :: placed).sortBy(x => (x.pos.row, x.pos.col))
  }

  /**
    * This is the main method of Solver trait.
    *
    * Runs placement method on given params and print the results in
    * human-friendly way.
    *
    * @param pieces list of pieces to be placed
    * @param cb chessboard where pieces will be placed
    * @return number of unique solutions for given params
    */
  def solve(pieces: List[String], cb: ChessBoard): Int = {
    val placement = placePieces(pieces, cb)

    placement.size
  }

  /**
    * This method prints the solution set in human-friendly format.
    *
    * @param solution set of solutions
    * @param cb chessboard where pieces are placed
    */
  def showSolution(solution: Set[List[ChessPiece]], cb: ChessBoard) = {
    println("Number of solutions: " + solution.size)

    for (sol <- solution) {
      println
      for (row <- 0 until cb.M) {
        val rowWithFigures =
          for {
            col <- 0 until cb.N
          } yield {
            if (sol.exists(x => x.pos.row == row && x.pos.col == col))
              sol.filter(x => x.pos.row == row && x.pos.col == col).head
            else "*"
          }

        println(rowWithFigures.mkString)
      }
    }

  }
}
