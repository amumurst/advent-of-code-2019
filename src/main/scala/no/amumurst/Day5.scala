package no.amumurst

object Day5 {
  lazy val run: Unit = {
    IntMachine(program = List(3, 0, 4, 0, 99), inputs = List(2)).advance.outputs
      .printAssert(List(2))

    IntMachine(program = readFileCsv("Day5.txt").toList, inputs = List(1)).advance.outputs
      .filterNot(_ == 0)
      .printTimed
  }
}

object Day5Part2 {
  lazy val run: Unit = {
    "Equal to 8".printed
    IntMachine(
      program = List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8),
      inputs = List(7)
    ).advance.outputs.printAssert(List(0))
    IntMachine(
      program = List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8),
      inputs = List(8)
    ).advance.outputs.printAssert(List(1))
    IntMachine(
      program = List(3, 3, 1108, -1, 8, 3, 4, 3, 99),
      inputs = List(7)
    ).advance.outputs.printAssert(List(0))
    IntMachine(
      program = List(3, 3, 1108, -1, 8, 3, 4, 3, 99),
      inputs = List(8)
    ).advance.outputs.printAssert(List(1))

    "Less than 8".printed
    IntMachine(
      program = List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8),
      inputs = List(8)
    ).advance.outputs.printAssert(List(0))
    IntMachine(
      program = List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8),
      inputs = List(5)
    ).advance.outputs.printAssert(List(1))
    IntMachine(
      program = List(3, 3, 1107, -1, 8, 3, 4, 3, 99),
      inputs = List(8)
    ).advance.outputs.printAssert(List(0))
    IntMachine(
      program = List(3, 3, 1107, -1, 8, 3, 4, 3, 99),
      inputs = List(5)
    ).advance.outputs.printAssert(List(1))

    "Equal to 0".printed
    IntMachine(
      program = List(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9),
      inputs = List(0)
    ).advance.outputs.printAssert(List(0))
    "here".printed
    IntMachine(
      program = List(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9),
      inputs = List(5)
    ).advance.outputs.printAssert(List(1))
    IntMachine(
      program = List(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1),
      inputs = List(0)
    ).advance.outputs.printAssert(List(0))
    IntMachine(
      program = List(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1),
      inputs = List(5)
    ).advance.outputs.printAssert(List(1))

    "BigEqual to 8".printed
    IntMachine(
      program = List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002,
        21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99),
      inputs = List(5)
    ).advance.outputs.printAssert(List(999))
    IntMachine(
      program = List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002,
        21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99),
      inputs = List(8)
    ).advance.outputs.printAssert(List(1000))
    IntMachine(
      program = List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002,
        21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99),
      inputs = List(9)
    ).advance.outputs.printAssert(List(1001))

    "program".printed
    IntMachine(program = readFileCsv("Day5.txt").toList, inputs = List(5)).advance.outputs
      .filterNot(_ == 0)
      .printTimed
  }
}
