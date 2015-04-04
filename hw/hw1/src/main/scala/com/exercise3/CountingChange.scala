package com.exercise3

/**
 * Created by zach on 3/7/15.
 */
object CountingChange {

  def countChange(money:Int, coins:List[Int]):Int = {
    if ( money == 0 ) 1
      else
        if (coins.isEmpty || money < coins.head) 0
        else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

  def main(args: Array[String]) {
    println(countChange(5, List(3,5,10)))
  }

}
