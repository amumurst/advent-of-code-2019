package no

package object amumurst {
  def readFileLines(name: String): LazyList[String] = scala.io.Source.fromResource(name).getLines().to(LazyList)
  def readFileCsvL(name: String): LazyList[Long]    = readFileLines(name).flatMap(_.split(',')).flatMap(_.toLongOption)
  def readFileCsv(name: String): LazyList[Int]      = readFileCsvL(name).map(_.toInt)

  implicit class StringOps(s: String) {
    def digits: List[Int] = s.toList.map(_.asDigit)
  }

  implicit class IntOps(i: Int) {
    def digits: List[Char] = i.toString.toList

    def addDigitChar(c: Char): Int = (i * 10) + c.asDigit
  }

  implicit class AnyOps[A](a: => A) {
    def printAssert(expected: A) = {
      if (a != expected)
        println(s"AssertError given: [$a], expected: [$expected]")
      else ()
    }

    def printed: A = {
      println(a)
      a
    }
    def printTimed: Unit = {
      val start = System.currentTimeMillis()
      val x     = a
      val end   = System.currentTimeMillis()
      println(s"Used ${(end.toDouble - start) / 1000.0} seconds")
      println(x)
    }
  }

}
