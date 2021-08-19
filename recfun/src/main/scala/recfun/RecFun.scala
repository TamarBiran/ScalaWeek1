package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()



  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if(c == 0 || c == r) 1 else (pascal(c-1 , r-1) + pascal(c, r-1))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    rec_balance(chars)

  def rec_balance(chars: List[Char], counter: Int = 0) : Boolean =
    if(counter == -1) false
    else if(chars.isEmpty)
      if(counter == 0) true else false
    else if(chars.head.equals('(')) rec_balance(chars.tail,counter+1)
    else if(chars.head.equals(')')) rec_balance(chars.tail,counter-1)
    else rec_balance(chars.tail,counter)
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if(coins.isEmpty) 0
    else if (money < 0 || coins.length < 0) 0
    else
      countChange(money-coins.head , coins) + countChange(money , coins.tail)
