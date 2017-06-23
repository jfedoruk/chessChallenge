package chessChallenge

import scala.annotation.tailrec

/**
  * This trait implements solver for Chess Challenge
  */
trait Solver {

  /**
    * Type for list of solutions
    */
  type Solutions = Set[List[ChessPiece]]

  /**
    * Check if given piece is safe to place among already placed pieces.
    *
    * @param piece piece to place
    * @param placed already placed pieces
    * @return True if piece is safe to place
    */
  def checkIfSafe(piece: ChessPiece, placed: List[ChessPiece]) : Boolean = {
    placed.forall(x => !x.isThreat(piece)) && placed.forall(x => !piece.isThreat(x))
  }

  /**
    * This method prints the solution set in human-friendly format.
    *
    * @param solution set of solutions
    * @param cb chessboard where pieces are placed
    */
  def showSolution(solution: Solutions, cb: ChessBoard) = {
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
    * @param piece piece to placed
    * @param alreadyPlaced already placed pieces
    * @param cb chessboard for challenge
    * @return list of pieces on safe fields
    */
  def findPlaceForPiece(piece: String, alreadyPlaced: List[ChessPiece], cb: ChessBoard) : List[ChessPiece] = {
    cb.freePlaces(alreadyPlaced)
      .filter(x => checkIfSafe(ChessPiece(piece, x), alreadyPlaced))
      .map(x => ChessPiece(piece, x))
      .toList
  }

  /**
    * Generate solution for selected piece
    *
    * @param piece piece to place
    * @param piecesToPlace pieces left to place
    * @param cb chessboard for challenge
    * @param alreadyPlaced pieces already placed
    * @return set of solutions
    */
  def generateSolutionForPiece(piece: ChessPiece, piecesToPlace: List[String], cb: ChessBoard, alreadyPlaced: List[ChessPiece]) : Solutions = {
    cb.occupiedFields = (piece :: alreadyPlaced).map(_.pos)
    if (piecesToPlace.isEmpty) Set((piece :: alreadyPlaced).sortBy(x => (x.pos.row, x.pos.col)))
    else placePieces(piecesToPlace, piece :: alreadyPlaced, cb)
  }

  /**
    * This method generate solutions for current situation on the chessboard.
    *
    * For each already generated solution method will try to put another piece.
    * If successful will generate a new solution including the piece.
    *
    * @param possiblePlaces possible placement of 1 piece
    * @param piecesToPlace pieces left to place
    * @param alreadyPlaced pieces already placed
    * @param cb chessboard for challenge
    * @return set of solutions for given input
    */
  def generateSolutions(
                         possiblePlaces: List[ChessPiece],
                         piecesToPlace: List[String],
                         alreadyPlaced: List[ChessPiece],
                         cb: ChessBoard) : Solutions = {

    @tailrec
    def innerGenerate(
                       possiblePlaces: List[ChessPiece],
                       piecesToPlace: List[String],
                       alreadyPlaced: List[ChessPiece],
                       cb: ChessBoard, solutions: Solutions) : Solutions = possiblePlaces match {
      case Nil => solutions
      case head :: tail =>
        val solutionsNew = generateSolutionForPiece(head, piecesToPlace, cb, alreadyPlaced)
        innerGenerate(tail, piecesToPlace, alreadyPlaced, cb, solutionsNew ++ solutions)
    }
    innerGenerate(possiblePlaces, piecesToPlace, alreadyPlaced, cb, Set())
  }


  /**
    * This method places all the pieces on given chessboard.
    *
    * Each piece is placed on the board one by one. Result of this operation is
    * given as input to next iteration.
    *
    * @param pieces pieces to place on chessboard
    * @param cb chessboard for challenge
    * @return set of solutions for given input
    */
  def placePieces(pieces: List[String], alreadyPlaced: List[ChessPiece], cb: ChessBoard) : Solutions =  pieces match {
      case Nil => Set()
      case head :: tail =>
        val possiblePlaces = findPlaceForPiece(head, alreadyPlaced, cb)
        generateSolutions(possiblePlaces, tail, alreadyPlaced, cb)
  }

  /**
    * This method calls a 2nd implementation of algorithm
    *
    * Runs placement method on given params and print the results in
    * human-friendly way.
    *
    * @param pieces list of pieces to be placed
    * @param cb chessboard where pieces will be placed
    * @return number of unique solutions for given params
    */
  def solve(pieces: List[String], cb: ChessBoard): (Int, Long) = {
    if (pieces.isEmpty) (0, 0)
    else {
      val t0 = System.nanoTime()
      val placement = placePieces(pieces, List(), cb)
      val timeElapsed = System.nanoTime() - t0

      (placement.size, timeElapsed)
    }
  }
}
