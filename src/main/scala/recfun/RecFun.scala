package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * If c = 0 that means we are in the first column and if c = r that means we are in the last column. In these cases
   * simply return 1
   * Otherwise add the number in the previous row and column (the number above and to the left) and add the number in
   * the previous row but same column (the number above and to the right)
   */
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * The basic mechanism is keeping track of how many times we encounter a left and right parentheses
   *  1. If we encounter a left one, we increment the counter
   *  2. If we encounter a right one, we decrement the counter
   * If the counter ends up being zero, that means that there is the same number of left and right parentheses, but in
   * order to also track that they apper in the right order we need to check that the counter never becomes negative,
   * because that would mean that we encountered a right parentheses before opening a left one. This leads to the
   * counter being equal to -1 thus distinct to zero and so we fail the process
   * We finish when there are no more characters left or we encounter the above situation
   */
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def countParentheses(chars: List[Char], counter: Int): Int = {
      if counter < 0 || chars.isEmpty then return counter
      chars.head match {
        case '(' => countParentheses(chars.tail, counter + 1)
        case ')' => countParentheses(chars.tail, counter - 1)
        case _ => countParentheses(chars.tail, counter)
      }
    }

    countParentheses(chars, 0) == 0
  }

  /**
   * The base cases for this exercise are the following:
   *  1. If we need to provide change for 0 money, the only thing we can do is return 0 coins, thus there is only one
   *     way of fulfilling the situation
   *  2. If we need to provide change for a negative amount of money, then there is no way of fulfilling that, thus 0
   *     ways
   *  3. If we don't have any coins and the money is greater than zero, then we can't give change at all, so 0 ways too
   * After that, when giving change, for a particular coin we can do two things:
   *  1. Don't use it to give change, thus removing it from the coin list, and try to give change using the remaining
   *     coins
   *  2. Include it in the change, thus subtracting the coin value from the money, keeping the coin and trying to give
   *     change again for the remaining money with the same coins
   * If we count how many ways we can give change doing the first thing plus how many ways we can do it doing the second
   * thing, and we apply it for each coin, then we get the total number of ways to give change
   * For more information check this problem on the internet xd and check the knapsack problem
   */
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if money == 0 then return 1
    if money < 0 then return 0
    if coins.isEmpty then return 0

    countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
