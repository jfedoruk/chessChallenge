package chessChallenge

/**
  * This trait represents a chess piece
  */
sealed trait ChessPiece {

  /** Position on the chess board */
  val pos: Position

  /**
    * Function to check if this piece is a threat for other piece on the board
    *
    * @param piece chess piece to check for threat
    * @return True if chess piece is at threat
    */
  def isThreat(piece: ChessPiece): Boolean

}

/**
  * This class represents position on chessboard.
  *
  * @param row row on chessboard
  * @param col column on chessboard
  */
case class Position(row: Int, col: Int)

/** Companion object for ChessPiece trait */
object ChessPiece {

  /**
    * Class to represent King chess piece
    *
    * @param pos piece position on chessboard
    */
  case class King(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = {
      Math.abs(piece.pos.col - pos.col) <= 1 && Math.abs(piece.pos.row - pos.row) <= 1
    }

    override def toString = "K"
  }

  /**
    * Class to represent Queen chess piece
    *
    * @param pos piece position on chessboard
    */
  case class Queen(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean =
      pos.row == piece.pos.row || pos.col == piece.pos.col ||
        Math.abs(piece.pos.row - pos.row) == Math.abs(piece.pos.col - pos.col)

    override def toString = "Q"
  }

  /**
    * Class to represent Bishop chess piece
    *
    * @param pos piece position on chessboard
    */
  case class Bishop(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean =
      Math.abs(piece.pos.row - pos.row) == Math.abs(piece.pos.col - pos.col)

    override def toString = "B"
  }

  /**
    * Class to represent Rook chess piece
    *
    * @param pos piece position on chessboard
    */
  case class Rook(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = pos.row == piece.pos.row || pos.col == piece.pos.col

    override def toString = "R"
  }

  /**
    * Class to represent Knight chess piece
    *
    * @param pos piece position on chessboard
    */
  case class Knight(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = {
      val knightRangePre =
        for {
          i <- -2 to 2
          j <- -2 to 2
          if i != 0 && j != 0 && Math.abs(i) != Math.abs(j)
        } yield Position(pos.row + i, pos.col + j)

      // Add current position to the list
      val knightRange = knightRangePre :+ Position(pos.row, pos.col)
      knightRange.contains(piece.pos)
    }

    override def toString = "N"
  }

  /**
    * This is a Factory method for ChessPiece
    *
    * @param s   name of the case class
    * @param pos position of chess piece
    * @return new object of given case class type
    */
  def apply(s: String, pos: Position): ChessPiece = s match {
    case "K" => King(pos)
    case "Q" => Queen(pos)
    case "B" => Bishop(pos)
    case "R" => Rook(pos)
    case "N" => Knight(pos)
    case _ => throw new RuntimeException // Something went wrong
  }

}