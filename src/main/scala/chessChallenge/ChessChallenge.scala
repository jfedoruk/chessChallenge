package chessChallenge

import scala.io.StdIn

/**
  * A main object that can be used to execute the ChessChallenge solver
  */
object ChessChallenge extends App {

  println("Welcome to Chess Challenge Solver!")
  val M = StdIn.readLine("Please provide the 1st dimension of the board M:")
  val N = StdIn.readLine("Please provide the 2nd dimension of the board N:")
  val pieces = StdIn.readLine("Please provide number of Kings, Queens, Bishops, Rooks and Knights separated by comma:")

  println()
  printf(s"Board is: $M x $N")
  println()
  println("Pieces are: " + pieces.split(",").toList.mkString(","))

  object solution extends Solver

  println("Number of solutions: " + solution.solution.length)
}
