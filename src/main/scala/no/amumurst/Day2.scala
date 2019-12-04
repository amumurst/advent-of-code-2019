package no.amumurst

object Day2 {

  def runProgram(list: List[Int], pos: Int): List[Int] = {
    list.drop(pos) match {
      case opcode :: rest if opcode == 99 => list
      case opcode :: readOne :: readTwo :: writeTo :: rest => {
        val value =
          if (opcode == 1) list(readOne) + list(readTwo) else if (opcode == 2) list(readOne) * list(readTwo) else 0
        runProgram(list.updated(writeTo, value), pos + 4)
      }
      case Nil => list
    }
  }

  def task(list: List[Int]): Int = runProgram(list.updated(1, 12).updated(2, 2), 0).head

  lazy val run: Unit = {
    runProgram(List(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), 0)
      .printAssert(List(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50))
    runProgram(List(2, 3, 0, 3, 99), 0).printAssert(List(2, 3, 0, 6, 99))
    runProgram(List(2, 4, 4, 5, 99, 0), 0).printAssert(List(2, 4, 4, 5, 99, 9801))
    runProgram(List(1, 1, 1, 4, 99, 5, 6, 0, 99), 0).printAssert(List(30, 1, 1, 4, 2, 5, 6, 0, 99))

    task(readFileCsv("Day2.txt").toList).printTimed
  }
}

object Day2Part2 {
  def solutionChecker(list: List[Int], one: Int, two: Int, solution: Int): Boolean =
    Day2.runProgram(list.updated(1, one).updated(2, two), 0).headOption.contains(solution)

  def task(solution: Int, file: String): Option[Int] = {
    val input = readFileCsv(file).toList

    val lookup = for {
      noun <- 1 to 100
      verb <- 1 to 100
    } yield (noun, verb)

    lookup
      .find { case (noun, verb) => solutionChecker(input, noun, verb, solution) }
      .map { case (noun, verb) => 100 * noun + verb }
  }

  lazy val run: Unit = task(19690720, "Day2.txt").printTimed
}
