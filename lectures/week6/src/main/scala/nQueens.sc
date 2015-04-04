object nQueens {

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val rows = queens.length - 1 to 0 -1
    val row = queens.length
    (rows zip queens).forall{ x => col!=x._2 && math.abs(col - x._2) != row - x._1 }
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col::queens
  }



}