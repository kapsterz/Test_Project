package recfun

object Main {
  def main(args: Array[String]) {
    /*    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }*/
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c <= 0) || (r <= 1) || (c >= r)) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */

  def n(chars: List[Char], x: Int): Boolean = {
    if (chars.isEmpty) {
      if (x != 0) false else true
    }
    else if (chars.head == '(') n(chars.tail, x + 1)
    else if (chars.head == ')') if (x != 0) n(chars.tail, x - 1) else false
    else n(chars.tail, x)
  }

  def balance(chars: List[Char]): Boolean = n(chars, 0)


  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0 && coins.nonEmpty) {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    else if (money == 0) 1 else 0
  }
}
