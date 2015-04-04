package com.exercise1

/**
 * Created by zach on 2/28/15.
 */
object PascalsTriangle {

  /**
   *
   * @param n
   * @param k
   * @return
   */
  def binomialCoeff(n: Int, k: Int):Int = {

    if (k == 0 || k == n) 1
    else binomialCoeff(n - 1, k - 1) + binomialCoeff(n - 1, k)

  }

  def pascal(depth: Int): Unit = {

    def printRow(row: Int, column: Int): Null = {
      if (column > row ) null
      else {
        print(binomialCoeff(row, column) + " ")
        printRow(row, column + 1)
      }

    }

    def printLoop(row:Int, depth:Int): Null = {
      if (row > depth -1) null
      else{
        print(" " * (depth - row))
        printRow(row, 0)
        print(" " * (depth - row -1) + "\n")
        printLoop(row + 1, depth)
      }
    }

    printLoop(0, depth)

  }

  def main (args: Array[String]) {
    val t0 = System.currentTimeMillis()
    pascal(5)
    println("time = " + (System.currentTimeMillis() - t0) + " ms")

  }

}
