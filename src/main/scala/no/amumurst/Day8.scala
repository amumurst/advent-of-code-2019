package no.amumurst

object Day8 {
  def program(input: LazyList[Int], width: Int, height: Int) =
    input
      .grouped(width * height)
      .to(LazyList)
      .sortBy(_.count(_ == 0))
      .headOption
      .map(l => l.count(_ == 1) * l.count(_ == 2))

  lazy val run: Unit = {
    program(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2), 3, 2).printAssert(Some(1))
    program(readFileLines("Day8.txt").flatMap(x => x.digits), 25, 6).printTimed
  }
}

object Day8Part2 {
  val Transparent = 2
  val White       = 1
  val Black       = 0

  def program(input: LazyList[Int], width: Int, height: Int) =
    input
      .grouped(width * height)
      .to(LazyList)
      .foldRight(LazyList.empty[Int]) {
        case (v, acc) =>
          acc.zipAll(v, Transparent, Transparent).map {
            case (under, Transparent) => under
            case (_, over)            => over
          }
      }

  def createImage(v: LazyList[Int], width: Int): String =
    v.grouped(width)
      .map(_.map {
        case Black => ' '
        case White => 'x'
        case n     => n.toChar
      }.mkString)
      .mkString("\n")

  lazy val run: Unit = {
    program(LazyList(0, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 0, 0, 0, 0), 2, 2).printAssert(LazyList(0, 1, 1, 0))
    val result = program(readFileLines("Day8.txt").flatMap(x => x.digits), 25, 6)
    createImage(result, 25).printTimed
  }
}
