package no.amumurst

import scala.collection.parallel.CollectionConverters._

object Day4 {
  def isAscending(s: List[Char]): Boolean = s.sliding(2).collect { case a :: b :: Nil => a <= b }.forall(identity)
  def isRepeat(s: List[Char]): Boolean    = s.sliding(2).collect { case a :: b :: Nil => a == b }.exists(identity)

  def numberIsValid(input: Int): Boolean = {
    val s = input.toString.toList
    isAscending(s) && isRepeat(s)
  }

  lazy val run = {
    printAssert(numberIsValid(111111), true)
    printAssert(numberIsValid(123455), true)
    printAssert(numberIsValid(111100), false)
    printAssert(numberIsValid(223450), false)
    printAssert(numberIsValid(123789), false)
    println((353096 to 843212).par.count(numberIsValid))
  }
}

object Day4Part2 {
  def dropGroupsLargerThan2(s: List[Char]): List[Char] =
    s match {
      case a :: b :: c :: rest if a == b && b != c => a +: b +: dropGroupsLargerThan2(c +: rest)
      case a :: b :: Nil                           => List(a, b)
      case a :: rest                               => dropGroupsLargerThan2(rest.dropWhile(_ == a))
      case a                                       => a
    }

  def numberIsValid(input: Int): Boolean = {
    val s = input.toString.toList
    Day4.isAscending(s) && Day4.isRepeat(dropGroupsLargerThan2(s))
  }

  lazy val run = {
    printAssert(numberIsValid(112233), true)
    printAssert(numberIsValid(123444), false)
    printAssert(numberIsValid(111122), true)
    println((353096 to 843212).par.count(numberIsValid))
  }
}
