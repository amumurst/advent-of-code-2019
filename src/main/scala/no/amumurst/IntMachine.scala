package no.amumurst

import scala.annotation.tailrec

case class IntMachine(pm: Map[Long, Long],
                      inputs: List[Long],
                      outputs: List[Long],
                      pos: Long,
                      name: String,
                      relativeBase: Long) {
  override def toString: String = s"M$name Inputs: $inputs Outputs:$outputs "

  def readValue(n: Long): Long   = pm.getOrElse(n, 0)
  def currentProgram: List[Long] = (pos to pos + 4).toList.map(readValue)
}

object IntMachine {
  def apply(program: List[Long], inputs: List[Long]): IntMachine =
    IntMachine(program.zipWithIndex.map(_.swap).map { case (i, l) => (i.toLong, l) }.toMap, inputs, Nil, 0, "a", 0)

  implicit class IntMachineOps(im: IntMachine) {
    //import im._
    implicit val self: IntMachine = im

    def updateProgram(value: Long, address: Long): IntMachine = im.copy(pm = im.pm.updated(address, value))
    def dropInpt: IntMachine                                  = im.copy(inputs = im.inputs.drop(1))
    def addOutput(o: Long): IntMachine                        = im.copy(outputs = im.outputs :+ o)
    def moveN(n: Long): IntMachine                            = im.copy(pos = im.pos + n)
    def moveRelativeBase(i: Long): IntMachine                 = im.copy(relativeBase = im.relativeBase + i)

    @tailrec
    final def advance: IntMachine = {
      im.currentProgram match {
        case Mode(Mode(a, b, c, 1)) :: readOne :: readTwo :: writeTo :: _ =>
          updateProgram(c.read(readOne) + b.read(readTwo), a.writeAd(writeTo)).moveN(4).advance
        case Mode(Mode(a, b, c, 2)) :: readOne :: readTwo :: writeTo :: _ =>
          updateProgram(c.read(readOne) * b.read(readTwo), a.writeAd(writeTo)).moveN(4).advance
        case Mode(Mode(_, _, c, 3)) :: writeTo :: _ =>
          im.inputs.headOption match {
            case Some(value) => dropInpt.updateProgram(value, c.writeAd(writeTo)).moveN(2).advance
            case None =>
              println(s"${im.name} has no more inputs, halting program")
              dropInpt
          }
        case Mode(Mode(_, _, c, 4)) :: readFrom :: _ =>
          addOutput(c.read(readFrom)).moveN(2).advance
        case Mode(Mode(_, b, c, 5)) :: readOne :: readTwo :: _ =>
          if (c.read(readOne) == 0) moveN(3).advance else moveN(b.read(readTwo) - im.pos).advance
        case Mode(Mode(_, b, c, 6)) :: readOne :: readTwo :: _ =>
          if (c.read(readOne) != 0) moveN(3).advance else moveN(b.read(readTwo) - im.pos).advance
        case Mode(Mode(a, b, c, 7)) :: readOne :: readTwo :: writeTo :: _ =>
          updateProgram(if (c.read(readOne) < b.read(readTwo)) 1 else 0, a.writeAd(writeTo)).moveN(4).advance
        case Mode(Mode(a, b, c, 8)) :: readOne :: readTwo :: writeTo :: _ =>
          updateProgram(if (c.read(readOne) == b.read(readTwo)) 1 else 0, a.writeAd(writeTo)).moveN(4).advance
        case Mode(Mode(_, _, c, 9)) :: readOne :: _ =>
          moveRelativeBase(c.read(readOne)).moveN(2).advance
        case 99 :: _ =>
          im
        case _ =>
          throw new UnsupportedOperationException(im.currentProgram.toString())
      }
    }

  }
  private sealed trait RMode extends Serializable with Product {
    def read(address: Long)(implicit im: IntMachine): Long = this match {
      case PositionalMode => im.readValue(address)
      case ImmidiateMode  => address
      case RelativeMode   => im.readValue(address + im.relativeBase)
    }
    def writeAd(address: Long)(implicit im: IntMachine): Long = this match {
      case PositionalMode | ImmidiateMode => address
      case RelativeMode                   => address + im.relativeBase
    }
  }
  private case object PositionalMode extends RMode
  private case object ImmidiateMode  extends RMode
  private case object RelativeMode   extends RMode

  private object RMode {
    def apply(i: Long): RMode = i match {
      case 1 => ImmidiateMode
      case 2 => RelativeMode
      case 0 => PositionalMode
    }
  }

  private case class Mode(a: RMode, b: RMode, c: RMode, opcode: Long)

  private object Mode {
    def unapply(i: Long): Option[Mode] =
      Some(Mode(RMode(i % 100000 / 10000), RMode(i % 10000 / 1000), RMode(i % 1000 / 100), i % 100))
  }
}
