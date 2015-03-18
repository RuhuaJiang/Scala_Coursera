package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) 1
    else {
      pascal(c / 2, r - 1) + pascal(c / 2 + 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCore(left: Int, right: Int, ls: List[Char]): Boolean = {
      if (right > left) false
      else if (ls.isEmpty) left == right
      else {
        if (ls.head != '(' && ls.head != ')') balanceCore(left, right, ls.tail)
        else if (ls.head == '(') balanceCore(left + 1, right, ls.tail)
        else balanceCore(left, right + 1, ls.tail)
      }
    }
    balanceCore(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money < 0) 0
    else if(coins.isEmpty && money > 0) 0
    else countChange(money-coins.head,coins) + countChange(money,coins.tail)
  }
}
