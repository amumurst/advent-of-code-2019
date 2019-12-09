package no.amumurst

object Day9 {

  lazy val run = {
    "copy".printed
    IntMachine(program = List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99), inputs = Nil).advance.outputs
      .printAssert(
        List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)
      )

    "16digit".printed
    IntMachine(program = List(1102, 34915192, 34915192, 7, 4, 7, 99, 0), inputs = Nil).advance.outputs
      .printAssert(List(1219070632396864L))

    "middlelarge".printed
    IntMachine(program = List(104, 1125899906842624L, 99), inputs = Nil).advance.outputs
      .printAssert(List(1125899906842624L))

    IntMachine(program = readFileCsvL("Day9.txt").toList, inputs = List(1)).advance.outputs.printTimed
  }
}

object Day9Part2 {
  lazy val run = IntMachine(program = readFileCsvL("Day9.txt").toList, inputs = List(2)).advance.outputs.printTimed
}
