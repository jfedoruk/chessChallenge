package chessChallenge

import scala.io.StdIn

/**
  * A main object that can be used to execute the ChessChallenge solver
  */
object ChessChallenge extends App {

  println("Welcome to Chess Challenge Solver!")
  try {
    val M = StdIn.readLine("Please provide the 1st dimension of the board M:").toInt
    val N = StdIn.readLine("Please provide the 2nd dimension of the board N:").toInt
    val kings = StdIn.readLine("Please provide number of Kings:").toInt
    val queens = StdIn.readLine("Please provide number of Queens:").toInt
    val bishops = StdIn.readLine("Please provide number of Bishops:").toInt
    val rooks = StdIn.readLine("Please provide number of Rooks:").toInt
    val knights = StdIn.readLine("Please provide number of Knights:").toInt

    println()
    printf(s"Board is: $M x $N")
    println()

    val chessBoardForChallenge = new ChessBoard(M, N)
    val pieces = List.fill(kings.toInt)("K") :::
      List.fill(queens.toInt)("Q") :::
      List.fill(bishops.toInt)("B") :::
      List.fill(rooks.toInt)("R") :::
      List.fill(knights.toInt)("N") ::: Nil

    object solution extends Solver
    val (nrSolutions, time) = solution.solve(pieces, chessBoardForChallenge)
    printf("Number of solutions: %d in %dns\n", nrSolutions, time)
  } catch {
    case exception: Throwable => println("Wrong input argument! Got exception: " + exception)
  }

}
