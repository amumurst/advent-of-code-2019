package no.amumurst

/*This solution is far too complicated and slow*/
case class WireStep(x: Int, y: Int, steps: Int) {
  lazy val pos = (x, y)

  override def equals(obj: Any): Boolean = obj match {
    case other: WireStep => this.pos == other.pos
    case _               => false
  }

  override lazy val hashCode: Int = pos.hashCode()

  @inline def moveX(dist: Int) = WireStep(x + dist, y, steps + dist.abs)
  @inline def moveY(dist: Int) = WireStep(x, y + dist, steps + dist.abs)

  def move(s: String): IndexedSeq[WireStep] = (s.headOption, s.tail.toIntOption) match {
    case (Some('R'), Some(l)) => (1 to l).map(x => moveX(x))
    case (Some('L'), Some(l)) => (1 to l).map(x => moveX(-x))
    case (Some('U'), Some(l)) => (1 to l).map(y => moveY(y))
    case (Some('D'), Some(l)) => (1 to l).map(y => moveY(-y))
    case _                    => println(s"bad format ${s}"); IndexedSeq.empty[WireStep]
  }

  @inline def manhatanDistanceFrom(other: WireStep): Int = (this.x - other.x).abs + (this.y - other.y).abs
}
object WireStep {
  val origin = WireStep(0, 0, 0)

  def getCoordSetForWire(wire: Seq[String])(move: WireStep => String => Seq[WireStep]): Seq[WireStep] = {
    def loop(wire: Seq[String], currentSet: Seq[WireStep]): Seq[WireStep] =
      wire match {
        case head :: rest => loop(rest, currentSet ++ move(currentSet.lastOption.getOrElse(origin))(head))
        case Nil          => currentSet
      }

    loop(wire, List(origin))
  }

  def wireSolver(a: List[String], b: List[String])(f: (Set[WireStep], Set[WireStep]) => Option[Int]) = {
    val aCordSet = WireStep.getCoordSetForWire(a)(_.move).filterNot(_ == WireStep.origin).toSet
    val bCordSet = WireStep.getCoordSetForWire(b)(_.move).filterNot(_ == WireStep.origin).toSet

    f(aCordSet, bCordSet)
  }
}

object Day3 {

  def program(a: List[String], b: List[String]): Option[Int] =
    WireStep.wireSolver(a, b) {
      case (aSet, bSet) => aSet.intersect(bSet).map(_.manhatanDistanceFrom(WireStep.origin)).minOption
    }

  def runProgram(fileName: String, program: (List[String], List[String]) => Option[Int]): Option[Int] =
    readFileLines(fileName).map(_.split(',').toList).toList match {
      case a :: b :: Nil => program(a, b)
      case _             => None
    }

  lazy val run = {
    program(List("R8", "U5", "L5", "D3"), List("U7", "R6", "D4", "L4")).printAssert(Some(6))
    program(List("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"),
            List("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")).printAssert(Some(159))
    program(List("R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51"),
            List("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7")).printAssert(Some(135))
    runProgram("Day3.txt", program).printTimed
  }
}

object Day3Part2 {

  def program(a: List[String], b: List[String]): Option[Int] =
    WireStep.wireSolver(a, b) {
      case (aSet, bSet) =>
        aSet
          .flatMap(as => bSet.filter(_.equals(as)).map(bs => (as, bs)))
          .map { case (a, b) => a.steps + b.steps }
          .minOption
    }

  lazy val run = {
    program(List("R8", "U5", "L5", "D3"), List("U7", "R6", "D4", "L4")).printAssert(Some(30))

    program(List("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"),
            List("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")).printAssert(Some(610))
    program(List("R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51"),
            List("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7")).printAssert(Some(410))
    Day3.runProgram("Day3.txt", program).printTimed
  }
}
