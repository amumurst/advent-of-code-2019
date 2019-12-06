package no.amumurst
import scala.collection.parallel.CollectionConverters._

object Day6 {

  def parseLines(ls: LazyList[String]): LazyList[(String, String)] =
    ls.map(s => (s.takeWhile(_ != ')'), s.dropWhile(_ != ')').drop(1)))

  type Astral = Map[String, List[String]]

  def astralFromLazyList(input: LazyList[(String, String)]): Astral = input.foldLeft[Astral](Map.empty) {
    case (acc, (asteroid, orbits)) =>
      acc.updatedWith(asteroid) {
        case Some(value) => Some(value :+ orbits)
        case None        => Some(List(orbits))
      }
  }

  def program(input: LazyList[(String, String)]): Int = {
    val xs = astralFromLazyList(input)

    def amountToCOM(s: String, n: Int): Int = xs.find { case (_, value) => value.contains(s) } match {
      case Some((key, _)) if key != "COM" => amountToCOM(key, n + 1)
      case _                              => n
    }

    xs.values.flatten.par
      .map(s => amountToCOM(s, 1))
      .sum
  }

  lazy val run = {
    val test1 = parseLines(LazyList("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"))

    program(test1).printAssert(42)

    val input = parseLines(readFileLines("Day6.txt"))
    program(input).printTimed
  }
}

object Day6Part2 {
  type CC = (String, Int)

  def program(input: LazyList[(String, String)]): Option[Int] = {
    val xs = Day6.astralFromLazyList(input)

    def canBeSeenFrom(c: CC): List[CC] = {
      val right = xs.find(_._1 == c._1).toList.flatMap(_._2).map(x => (x, c._2 + 1))
      val left  = xs.filter { case (_, value) => value.contains(c._1) }.keySet.toList.map(x => (x, c._2 + 1))
      left ++ right
    }
    def diffCC(a: List[CC], b: List[CC]): List[CC] = a.filterNot(x => b.map(_._1).contains(x._1))

    def loop(visited: List[CC], seen: List[CC]): List[CC] = {
      seen match {
        case a :: tail => loop(visited :+ a, tail ++ diffCC(canBeSeenFrom(a), visited))
        case _         => visited
      }
    }

    loop(List.empty, List(("YOU", 0))).find(_._1 == "SAN").map(_._2).map(_ - 2)
  }

  lazy val run = {
    val test1 = Day6.parseLines(
      LazyList("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"))

    program(test1).printAssert(Some(4))

    val input = Day6.parseLines(readFileLines("Day6.txt"))
    program(input).printTimed
  }
}
