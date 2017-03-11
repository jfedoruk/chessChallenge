package chessChallenge

/**
  * This is abstract class that represent a chess piece
  */
abstract class ChessPiece {

  /** Position on the chess board */
  val pos: Position

  /** Function to check if this piece is a threat for other piece on the board */
  def isThreat(piece: ChessPiece): Boolean

}

case class Position(row: Int, col: Int)

/**
  * Class to represent King chess piece
  */
case class King(pos: Position) extends ChessPiece {
  def isThreat(piece: ChessPiece): Boolean = ???
}

/**
  * Class to represent Queen chess piece
  */
case class Queen(pos: Position) extends ChessPiece {
  def isThreat(piece: ChessPiece): Boolean = ???
}

/**
  * Class to represent Bishop chess piece
  */
case class Bishop(pos: Position) extends ChessPiece {
  def isThreat(piece: ChessPiece): Boolean = ???
}

/**
  * Class to represent Rook chess piece
  */
case class Rook(pos: Position) extends ChessPiece {
  def isThreat(piece: ChessPiece): Boolean = ???
}

/**
  * Class to represent Knight chess piece
  */
case class Knight(pos: Position) extends ChessPiece {
  def isThreat(piece: ChessPiece): Boolean = ???
}