package chessChallenge

/**
  * This class represent a chessboard
  *
  * @param M number of rows
  * @param N number of cols
  */
class ChessBoard(var M: Int, var N: Int) {

  /**
    * Chess board is represented as a function from piece position
    * to boolean.
    * Function returns `true` if given position is inside the
    * chess board.
    */
  type ChessBoardType = Position => Boolean

  /**
    * Chess board function to test if given position is inside the board
    *
    * @param M number of rows
    * @param N number of cols
    * @return function from Position to Boolean
    */
  def chessBoardFunction(M: Int, N: Int) : Position => Boolean = pos => {
    if (pos.row > M) false
    else if (pos.col > N) false
    else true
  }

  /**
    * Return sequence of free chessboard fields
    *
    * @param placed chess pieces placed
    * @return Sequence of free Positions
    */
  def freePlaces(placed: List[ChessPiece]) : Seq[Position] = {
    chessBoardFields diff occupiedFields filter(
      x => !placed.exists(y => y.pos.row == x.row && y.pos.col == x.col))
  }

  /** Fields already occupied */
  var occupiedFields: Seq[Position] = Seq()

  /** All chess board fields */
  val chessBoardFields: Seq[Position] = {
    for {
      i <- 0 until M
      j <- 0 until N
    } yield Position(i, j)
  }

  /** Chess board for the challenge */
  val chessBoard: ChessBoardType = chessBoardFunction(M, N)
}

