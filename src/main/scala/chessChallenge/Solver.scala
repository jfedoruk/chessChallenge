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
  def placePieces(pieces: List[String], cb: ChessBoard): Solutions =  pieces match {
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
    * @param piece piece to place
    * @param placed already placed pieces
    * @return True if piece is safe to place
    */
  def checkIfSafe(piece: ChessPiece, placed: List[ChessPiece]) : Boolean = {
    placed.forall(x => !x.isThreat(piece)) && placed.forall(x => !piece.isThreat(x))
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
  def solve(pieces: List[String], cb: ChessBoard): (Int, Long) = {
    val t0 = System.nanoTime()
    val placement = placePieces(pieces, cb)
    val timeElapsed = System.nanoTime() - t0

    (placement.size, timeElapsed)
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
    * This functions place piece on the given chess board.
    *
    * Piece will be placed only if it does not threat other
    * already placed pieces. Also the piece cannot threat other
    * pieces after taking the position on the board.
    *
    * Result of the operation is sorted to eliminate duplications.
    *
    * @param piece piece to be placed
    * @param alreadyPlaced pieces already placed
    * @param cb chessboard for challenge
    * @return set of solutions for given input
    */
  def placePieceForSolution(piece: String, alreadyPlaced: List[ChessPiece], cb: ChessBoard) : Solutions = {
    cb.freePlaces(alreadyPlaced).filter(x => checkIfSafe(ChessPiece(piece, x), alreadyPlaced)) match {
      case Nil => Set[List[ChessPiece]]()
      case newPlacement =>
        newPlacement.map(x => (ChessPiece(piece, x) :: alreadyPlaced)
          .sortBy(y => (y.pos.row, y.pos.col)))
          .toSet
    }
  }

  /**
    * This method generate solutions for current situation on the chessboard.
    *
    * For each already generated solution method will try to put another piece.
    * If successful will generate a new solution including the piece.
    *
    * @param piece piece to be placed
    * @param solutions set of solutions produced in previous step
    * @param alreadyPlaced pieces already placed
    * @param cb chessboard for challenge
    * @return set of solutions for given input
    */
  def generateSolutions(
                         piece: String,
                         solutions: Solutions,
                         alreadyPlaced: List[ChessPiece],
                         cb: ChessBoard): Solutions = {

    @tailrec
    def innerGenerateSolution(piece: String,
                              solutions: Solutions,
                              alreadyPlaced: List[ChessPiece],
                              cb: ChessBoard,
                              newSolutions: Solutions): Solutions = solutions.toList match {
        case Nil =>
          if (newSolutions.isEmpty) placePieceForSolution(piece, alreadyPlaced, cb)
          else newSolutions
        case singleSolution :: solutionsTail =>
          val placed = placePieceForSolution(piece, singleSolution, cb).filter(_.length != alreadyPlaced.length)
          innerGenerateSolution(piece, solutionsTail.toSet, singleSolution, cb, placed ++ newSolutions)
      }
    innerGenerateSolution(piece, solutions, alreadyPlaced, cb, Set[List[ChessPiece]]())
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
  def placePieces2(pieces: List[String], cb: ChessBoard) : Solutions = {
    @tailrec
    def doPlace(pieces: List[String], solutions: Solutions) : Solutions = pieces match {
      case Nil => solutions
      case head :: tail =>
        val solutionsTemp: Solutions = generateSolutions(head, solutions, List(), cb)
        doPlace(tail, solutionsTemp)
    }
    doPlace(pieces.sorted, Set[List[ChessPiece]]())
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
  def solve2(pieces: List[String], cb: ChessBoard): (Int, Long) = {
    val t0 = System.nanoTime()
    val placement = placePieces2(pieces, cb)
    val timeElapsed = System.nanoTime() - t0

    (placement.size, timeElapsed)
  }
}
