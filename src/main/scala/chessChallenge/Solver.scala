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
        if placed.forall(x => !x.isThreat(piece))
      } yield piece :: placed
  }

  def solve(pieces: List[String], cb: ChessBoard): Int = {
    val placement = placePieces(pieces, cb)

    placement.toList.length
  }

  def showSolution(solution: Set[List[ChessPiece]], cb: ChessBoard) = {
    val board =
      for {
        row <- 0 until cb.M
      } yield Vector.fill(cb.N)("*").mkString + "\n"

    val boardWithFigures =
    for {
      sol <- solution
      piece <- sol
    } yield board.updated(piece.pos.col, piece).mkString

    boardWithFigures
  }
}
