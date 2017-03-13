package chessChallenge

/**
  * This class represent a chess board
  * Input:
  *  M - number of rows
  *  N - number of columns
  */
class ChessBoard(var M: Int, var N: Int) {

  /**
    * Chess board is represented as a function from piece position
    * to boolean.
    * Function returns `true` if given position is inside the
    * chess board.
    */
  type ChessBoardType = Position => Boolean

  /** Chess board function to test if given position is inside the board */
  def chessBoardFunction(M: Int, N: Int) : Position => Boolean = pos => {
    if (pos.row > M) false
    else if (pos.col > N) false
    else true
  }

  /** Chess board for the challenge */
  val chessBoard: ChessBoardType = chessBoardFunction(M, N)

}
