package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def loop(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1
      else loop(c - 1, r - 1) + loop(c, r - 1)

    loop(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        acc == 0;
      }
      else if (chars.head == ')' && acc == 0) {
        false;
      }

      // ---------------------

      else if (chars.head == ')') {
        loop(acc - 1, chars.tail);
      }
      else if (chars.head == '(') {
        loop(acc + 1, chars.tail);
      }

      else loop(acc, chars.tail);
    }

    loop(0, chars);
  }

  /**
   * Exercise 3

  def countChange(money: Int, coins: List[Int]): Int = {
    var countOfChange = 0

    def loop(acc: Int, coins: List[Int], actualMass: List[Int]): Unit = {
      if (acc == 0) {
        countOfChange = countOfChange + 1
      }
      else if (acc > 0 && !coins.isEmpty)
        for (coin <- coins) {
          loop(acc - coin, coins.slice(coins.indexOf(coin), coins.size), actualMass ++ List(coin))
        }
    }

    if (money == 0 || coins == 0) 0
    else {
      loop(money, coins, List.empty)
      countOfChange
    }
  }
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }

}
