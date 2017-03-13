package chessChallenge

/**
  * This trait represents a chess piece
  */
sealed trait ChessPiece {

  /** Position on the chess board */
  val pos: Position

  /** Function to check if this piece is a threat for other piece on the board */
  def isThreat(piece: ChessPiece): Boolean

}

case class Position(row: Int, col: Int)

/** Companion object for ChessPiece trait */
object ChessPiece {

  /**
    * Class to represent King chess piece
    */
  case class King(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = {
      val kingsRange =
        for {
        i <- -1 to 1
        j <- -1 to 1
      } yield Position(pos.row + i, pos.col + j)

      kingsRange.contains(piece.pos)
    }

    override def toString = "K"
  }

  /**
    * Class to represent Queen chess piece
    */
  case class Queen(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = ???

    override def toString = "Q"
  }

  /**
    * Class to represent Bishop chess piece
    */
  case class Bishop(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = ???

    override def toString = "B"
  }

  /**
    * Class to represent Rook chess piece
    */
  case class Rook(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = pos.row == piece.pos.row || pos.col == piece.pos.col

    override def toString = "R"
  }

  /**
    * Class to represent Knight chess piece
    */
  case class Knight(pos: Position) extends ChessPiece {
    def isThreat(piece: ChessPiece): Boolean = {
      val knightRangePre =
        for {
          i <- -2 to 2
          j <- -2 to 2
          if i != 0 && j!= 0 && Math.abs(i) != Math.abs(j)
        } yield Position(pos.row + i, pos.col + j)

      // Add current position to the list
      val knightRange = knightRangePre :+ Position(pos.row, pos.col)
      knightRange.contains(piece.pos)
    }

    override def toString = "N"
  }

  def apply(s: String, pos: Position): ChessPiece = s match {
    case "K" => new King(pos)
    case "Q" => new Queen(pos)
    case "B" => new Bishop(pos)
    case "R" => new Rook(pos)
    case "N" => new Knight(pos)
    case _ => throw new RuntimeException // Something went wrong
  }

}