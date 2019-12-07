package no.amumurst

case class IntMachine(program: List[Int] = Nil, inputs: List[Int] = Nil, outputs: List[Int] = Nil, pos: Int = 0)

object IntMachine {
  implicit class IntMachineOps(im: IntMachine) {
    import im._

    def updateProgram(value: Int, address: Int): IntMachine = copy(program = program.updated(address, value))
    def takeInput: (Option[Int], IntMachine)                = (inputs.headOption, copy(inputs = inputs.drop(1)))
    def addOutput(o: Int): IntMachine                       = copy(outputs = outputs :+ o)
    def moveN(n: Int): IntMachine                           = copy(pos = pos + n)
    def readValue(n: Int): Int                              = program(n)

    lazy val advance: IntMachine =
      program.drop(pos) match {
        case Mode(Mode(_, b, c, 1)) :: readOne :: readTwo :: writeTo :: _ =>
          val p1   = if (c) readOne else readValue(readOne)
          val p2   = if (b) readTwo else readValue(readTwo)
          val next = updateProgram(p1 + p2, writeTo).moveN(4)
          next.advance
        case Mode(Mode(_, b, c, 2)) :: readOne :: readTwo :: writeTo :: _ =>
          val p1   = if (c) readOne else readValue(readOne)
          val p2   = if (b) readTwo else readValue(readTwo)
          val next = updateProgram(p1 * p2, writeTo).moveN(4)
          next.advance
        case Mode(Mode(_, _, _, 3)) :: writeTo :: _ =>
          val (inputValue, newP) = takeInput
          val next = inputValue match {
            case Some(value) => newP.updateProgram(value, writeTo).moveN(2)
            case None        => newP.moveN(2)
          }
          next.advance
        case Mode(Mode(_, _, c, 4)) :: readFrom :: _ =>
          val value = if (c) readFrom else readValue(readFrom)
          val next  = addOutput(value).moveN(2)
          next.advance
        case Mode(Mode(_, b, c, 5)) :: readOne :: readTwo :: _ =>
          val value   = if (c) readOne else readValue(readOne)
          lazy val np = if (b) readTwo else readValue(readTwo)
          val next    = if (value == 0) moveN(3) else moveN(np - pos)
          next.advance
        case Mode(Mode(_, b, c, 6)) :: readOne :: readTwo :: _ =>
          val value   = if (c) readOne else readValue(readOne)
          lazy val np = if (b) readTwo else readValue(readTwo)
          val next    = if (value != 0) moveN(3) else moveN(np - pos)
          next.advance
        case Mode(Mode(_, b, c, 7)) :: readOne :: readTwo :: writeTo :: _ =>
          val value  = if (c) readOne else readValue(readOne)
          val check  = if (b) readTwo else readValue(readTwo)
          val stores = if (value < check) 1 else 0
          val next   = updateProgram(stores, writeTo).moveN(4)
          next.advance
        case Mode(Mode(_, b, c, 8)) :: readOne :: readTwo :: writeTo :: _ =>
          val value  = if (c) readOne else readValue(readOne)
          val check  = if (b) readTwo else readValue(readTwo)
          val stores = if (value == check) 1 else 0
          val next   = updateProgram(stores, writeTo).moveN(4)
          next.advance
        case 99 :: _ => im
        case _       => throw new UnsupportedOperationException(program.drop(pos).toString())
      }
  }

  private case class Mode(a: Boolean, b: Boolean, c: Boolean, opcode: Int)

  private object Mode {
    def unapply(i: Int): Option[Mode] =
      Some(Mode((i % 100000 / 10000) > 0, (i % 10000 / 1000) > 0, (i % 1000 / 100) > 0, i % 100))
  }
}
