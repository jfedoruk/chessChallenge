package chessChallenge

/**
  * This trait represent a chess board
  */
trait ChessBoard extends Position{

  /**
    * Chess board is represented as a function from piece position
    * to boolean.
    * Function returns `true` if given position is inside the
    * chess board.
    */
  type ChessBoard = Position => Boolean

  /** Chess board for the challenge */
  val chessBoard: ChessBoard
}
