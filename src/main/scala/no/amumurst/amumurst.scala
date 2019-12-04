package no

package object amumurst {
  def readFileLines(name: String): LazyList[String] = scala.io.Source.fromResource(name).getLines().to(LazyList)
  def readFileCsv(name: String): LazyList[Int] =
    scala.io.Source.fromResource(name).getLines().flatMap(_.split(',')).flatMap(_.toIntOption).to(LazyList)

  def printAssert[T](given: T, expected: T): Unit = {
    if (given != expected)
      println(s"AssertError given: [$given], expected: [$expected]")
    else ()
  }

  implicit class IntOps(i: Int) {
    def digits: List[Char] = i.toString.toList

    def addNumberChar(c: Char): Int = (i * 10) + c.asDigit
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
    def timed: A = {
      val start = System.currentTimeMillis()
      val x     = a
      val end   = System.currentTimeMillis()
      println(s"Used ${(end.toDouble - start) / 1000.0} seconds")
      x
    }
    def printTimed: A = timed.printed
  }

}
