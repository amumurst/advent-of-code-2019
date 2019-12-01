package no.amumurst

object Day1 {
  def fuelForMass(mass: Int): Int = (mass / 3) - 2

  printAssert(fuelForMass(12), 2)
  printAssert(fuelForMass(14), 2)
  printAssert(fuelForMass(1969), 654)
  printAssert(fuelForMass(100756), 33583)

  def run: Unit = {
    val input  = readFileLines("Day1.txt").flatMap(_.toIntOption)
    val result = input.map(fuelForMass).sum
    println(result)
  }
}

object Day1Part2 {
  def fuelForMass(mass: Int): Int = {
    val fuel = (mass / 3) - 2
    if (fuel > 0) fuel + fuelForMass(fuel) else 0
  }

  printAssert(fuelForMass(14), 2)
  printAssert(fuelForMass(1969), 966)
  printAssert(fuelForMass(100756), 50346)

  def run: Unit = {
    val input  = readFileLines("Day1.txt").flatMap(_.toIntOption)
    val result = input.map(fuelForMass).sum
    println(result)
  }

}
