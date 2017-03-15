package chessChallenge

import scala.io.StdIn

/**
  * A main object that can be used to execute the ChessChallenge solver
  */
object ChessChallenge extends App {

  println("Welcome to Chess Challenge Solver!")
  val M = StdIn.readLine("Please provide the 1st dimension of the board M:")
  val N = StdIn.readLine("Please provide the 2nd dimension of the board N:")
  val kings = StdIn.readLine("Please provide number of Kings:")
  val queens = StdIn.readLine("Please provide number of Queens:")
  val bishops = StdIn.readLine("Please provide number of Bishops:")
  val rooks = StdIn.readLine("Please provide number of Rooks:")
  val knights = StdIn.readLine("Please provide number of Knights:")

  println()
  printf(s"Board is: $M x $N")
  println()

  val chessBoardForChallenge = new ChessBoard(M.toInt, N.toInt)
  val pieces = List.fill(kings.toInt)("K") :::
               List.fill(queens.toInt)("Q") :::
               List.fill(bishops.toInt)("B") :::
               List.fill(rooks.toInt)("R") :::
               List.fill(knights.toInt)("N") ::: Nil

  object solution extends Solver
  val (nrSolutions, time) =  solution.solve(pieces, chessBoardForChallenge)
  printf("Number of solutions: %d in %dns\n", nrSolutions, time)
}
