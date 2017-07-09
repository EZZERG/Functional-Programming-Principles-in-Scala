object session {

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (c == r)) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def is_balanced(string1: List[Char], opens: Int): Boolean = {
      if (string1.isEmpty) (opens == 0)
      else {
        if (string1.head == '(') is_balanced(string1.tail, opens + 1)
        else if (string1.head == ')') (opens > 0) && is_balanced(string1.tail, opens - 1)
        else is_balanced(string1.tail, opens)
      }
    }

    is_balanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || (money < 0)) 0
    else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }


  println("Pascal's Triangle")
  for (row <- 0 to 10) {
    for (col <- 0 to row)
      println(pascal(col, row) + " ")
    println()
  }

  balance("(if (zero? x) max (/ 1 x))".toList)
  balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
  balance(":-)".toList)
  balance("())(".toList)

  countChange(4, List(1,2))
}