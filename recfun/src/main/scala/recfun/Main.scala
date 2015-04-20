package recfun
import common._
import scala.math.pow
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
    if (c == r || c == 0)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check_stack(stack: List[Char], left: List[Char]): Boolean = {
      if (left.isEmpty)
        stack.isEmpty
      else
        left.head match {
          case c if c == ')' =>
            if (stack.isEmpty)
              false
            else
              check_stack(stack.tail, left.tail)
          case c if c == '(' => check_stack(c::stack, left.tail)
          case _ => check_stack(stack, left.tail)
        }
    }
    check_stack(List(), chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(v: Int, k: Int): Stream[Int] = (pow(2, v).intValue * k) #:: loop(v + 1, k)

    def makeCoins(coin: Int): List[Int] = {
      loop(0, coin).takeWhile((value: Int) => value <= money).toList
    }

    def countChangeUnique(amount: Int, uniqueCoins: List[Int]): Int = {
      if (uniqueCoins.isEmpty)
        if (amount == 0)
          1
        else
          0
      else
        amount match {
          case i if i < 0 => 0
          case i if i == 0 => 1
          case i if i > 0 => countChangeUnique(amount, uniqueCoins.tail) + countChangeUnique(amount - uniqueCoins.head, uniqueCoins.tail)
        }
    }

    countChangeUnique(money, coins.flatMap(makeCoins))
//
//    if (coins.isEmpty)
//      0
//    else
//      money match {
//        case i if i < 0 => 0
//        case i if i == 0 => 1
//        case i if i > 0 => countChange(money, coins.tail) + countChange(money - coins.head, coins)
//      }
  }
}
