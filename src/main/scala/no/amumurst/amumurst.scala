package no

package object amumurst {
  def readFileLines(name: String): LazyList[String] = scala.io.Source.fromResource(name).getLines().to(LazyList)

  def printAssert[T](given: T, expected: T): Unit = {
    if (given != expected)
      println(s"AssertError given: [$given], expected: [$expected]")
    else ()
  }
}
