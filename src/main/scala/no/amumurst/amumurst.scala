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
}
