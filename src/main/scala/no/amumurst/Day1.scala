package no.amumurst

object Day1 {
  def fuelForMass(mass: Int): Int = (mass / 3) - 2

  def program(filename: String) = {
    val input = readFileLines(filename).flatMap(_.toIntOption)
    input.map(fuelForMass).sum
  }

  lazy val run: Unit = {
    fuelForMass(12).printAssert(2)
    fuelForMass(14).printAssert(2)
    fuelForMass(1969).printAssert(654)
    fuelForMass(100756).printAssert(33583)
    program("Day1.txt").printTimed
  }
}

object Day1Part2 {
  def fuelForMass(mass: Int): Int = {
    val fuel = (mass / 3) - 2
    if (fuel > 0) fuel + fuelForMass(fuel) else 0
  }

  def program(filename: String) = {
    val input = readFileLines("Day1.txt").flatMap(_.toIntOption)
    input.map(fuelForMass).sum
  }

  lazy val run: Unit = {
    fuelForMass(14).printAssert(2)
    fuelForMass(1969).printAssert(966)
    fuelForMass(100756).printAssert(50346)
    program("Day1.txt").printTimed
  }

}
