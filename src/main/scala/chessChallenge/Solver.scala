package chessChallenge

/**
  * This trait implements solver for Chess Challenge
  */
trait Solver {

  /**
    * Type for list of solutions
    */
  type Solutions = Set[List[ChessPiece]]

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
    * @param cb     chessboard where pieces will be placed
    * @return set of solutions for given input params
    */
  def placePieces(pieces: List[String], cb: ChessBoard): Solutions = pieces match {
    case Nil => Set(List())
    case head :: tail =>
      for {
        placed <- placePieces(tail, cb)
        position <- cb.freePlaces(placed)
        piece = ChessPiece(head, position)
        if checkIfSafe(piece, placed)
      } yield (piece :: placed).sortBy(x => (x.pos.row, x.pos.col))
  }

  /**
    * Check if given piece is safe to place among already placed pieces.
    *
    * @param piece  piece to place
    * @param placed already placed pieces
    * @return True if piece is safe to place
    */
  def checkIfSafe(piece: ChessPiece, placed: List[ChessPiece]): Boolean = {
    placed.forall(x => !x.isThreat(piece)) && placed.forall(x => !piece.isThreat(x))
  }

  /**
    * This is the main method of Solver trait.
    *
    * Runs placement method on given params and print the results in
    * human-friendly way.
    *
    * @param pieces list of pieces to be placed
    * @param cb     chessboard where pieces will be placed
    * @return number of unique solutions for given params
    */
  def solve(pieces: List[String], cb: ChessBoard): (Int, Long) = {
    if (pieces.isEmpty) (0, 0)
    else {
      val t0 = System.nanoTime()
      val placement = placePieces(pieces, cb)
      val timeElapsed = System.nanoTime() - t0
      (placement.size, timeElapsed)
    }
  }

  /**
    * This method prints the solution set in human-friendly format.
    *
    * @param solution set of solutions
    * @param cb       chessboard where pieces are placed
    */
  def showSolution(solution: Solutions, cb: ChessBoard): Unit = {
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

  /**
    * Finds fields where given piece can be placed
    *
    * @param piece         piece to placed
    * @param alreadyPlaced already placed pieces
    * @param cb            chessboard for challenge
    * @return list of pieces on safe fields
    */
  def findPlaceForPiece(piece: String, alreadyPlaced: List[ChessPiece], cb: ChessBoard): List[ChessPiece] = {
    cb.freePlaces(alreadyPlaced)
      .filter(x => checkIfSafe(ChessPiece(piece, x), alreadyPlaced))
      .map(x => ChessPiece(piece, x))
      .toList
  }

}
