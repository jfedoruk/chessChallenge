package chessChallenge

/**
  * This trait implements solver for Chess Challenge
  */
trait Solver {

  /**
    * Holds list of solutions
    */
  val solution: Set[List[ChessPiece]] = Set(List())

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

  def solve(pieces: List[String], cb: ChessBoard): Int = {
    val placement = placePieces(pieces, cb)

    showSolution(placement, cb)

    placement.size
  }

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

        println(rowWithFigures mkString)
      }
    }

  }
}
