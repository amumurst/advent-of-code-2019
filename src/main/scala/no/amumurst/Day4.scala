package no.amumurst

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Day4 {
  def isAscending(i: Int): Boolean =
    i.digits.sliding(2).collect { case a :: b :: Nil => a <= b }.forall(identity)
  def isRepeat(i: Int): Boolean = i.digits.sliding(2).collect { case a :: b :: Nil => a == b }.exists(identity)

  def numberIsValid(input: Int): Boolean =
    isAscending(input) && isRepeat(input)

  lazy val run = {
    numberIsValid(111111).printAssert(true)
    numberIsValid(123455).printAssert(true)
    numberIsValid(111100).printAssert(false)
    numberIsValid(223450).printAssert(false)
    numberIsValid(123789).printAssert(false)
    (353096 to 843212).par.count(numberIsValid).printTimed
  }
}

object Day4Part2 {
  def dropGroupsLargerThan2(i: Int): Int = {
    @tailrec
    def loop(s: List[Char], number: Int): Int =
      s match {
        case Nil                                     => number
        case a :: b :: c :: rest if a == b && b != c => loop(c +: rest, number.addDigitChar(a).addDigitChar(b))
        case a :: b :: Nil                           => number.addDigitChar(a).addDigitChar(b)
        case a :: rest                               => loop(rest.dropWhile(_ == a), number)
      }

    loop(i.digits, 0)
  }

  def numberIsValid(input: Int): Boolean =
    Day4.isAscending(input) && Day4.isRepeat(dropGroupsLargerThan2(input))

  lazy val run = {
    numberIsValid(112233).printAssert(true)
    numberIsValid(123444).printAssert(false)
    numberIsValid(111122).printAssert(true)
    (353096 to 843212).par.count(numberIsValid).printTimed
  }
}
