package com.exercise2

/**
 * Created by zach on 3/7/15.
 */
object Balance {

  def balance(chars: List[Char]) : Boolean = {

    def balance(chars: List[Char], open: Int):Boolean = {

      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balance(chars.tail, open+1)
            else if (chars.head == ')') open>0 && balance(chars.tail, open-1)
                  else balance(chars.tail, open)
    }

    balance(chars, 0)

  }

  def main(args: Array[String]) {
    println(balance("(if (zero? x) max (/ 1 x))".toList))
  }
}
