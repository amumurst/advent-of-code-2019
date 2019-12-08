package no.amumurst

object Day7 {

  def checkPhase(program: List[Int], phases: List[Int]): Int =
    phases.foldLeft(List(0)) { case (acc, v) => IntMachine(program = program, inputs = v +: acc).advance.outputs }.head

  def program(input: List[Int]): Option[Int] = (0 to 4).toList.permutations.map(checkPhase(input, _)).maxOption

  lazy val run = {
    program(input = List(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0))
      .printAssert(Some(43210))

    program(List(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0))
      .printAssert(Some(54321))

    program(
      List(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31,
        31, 4, 31, 99, 0, 0, 0))
      .printAssert(Some(65210))

    program(readFileCsv("Day7.txt").toList).printTimed
  }
}

object Day7Part2 {

  def program(input: List[Int]): Option[Int] = {

    val x  = (5 to 9).toList.permutations
    val x2 = Iterator(List(9, 8, 7, 6, 5))

    def outputs(phaseSettings: LazyList[Int]): LazyList[Int] =
      phaseSettings.foldLeft(0 #:: outputs(phaseSettings)) { (inputs, phaseSetting) =>
        IntMachine(program = input, inputs = (phaseSetting #:: inputs).toList).advance.outputs.to(LazyList)
      }

    x2.map(s => outputs(s.to(LazyList)).last).maxOption
  }

  def run = {

    val res = program(
      input = List(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6,
        99, 0, 0, 5))
    res.printed
    assert(res == Some(139629729))
  }
}
