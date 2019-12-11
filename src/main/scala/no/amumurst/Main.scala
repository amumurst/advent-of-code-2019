package no.amumurst

object Main extends App {
  args.headOption match {
    case Some("1")   => Day1.run
    case Some("1b")  => Day1Part2.run
    case Some("2")   => Day2.run
    case Some("2b")  => Day2Part2.run
    case Some("3")   => Day3.run
    case Some("3b")  => Day3Part2.run
    case Some("4")   => Day4.run
    case Some("4b")  => Day4Part2.run
    case Some("5")   => Day5.run
    case Some("5b")  => Day5Part2.run
    case Some("6")   => Day6.run
    case Some("6b")  => Day6Part2.run
    case Some("7")   => Day7.run
    case Some("7b")  => Day7Part2.run
    case Some("8")   => Day8.run
    case Some("8b")  => Day8Part2.run
    case Some("9")   => Day9.run
    case Some("9b")  => Day9Part2.run
    case Some("10")  => Day10.run
    case Some("10b") => Day10Part2.run
    case _           => println("No day selected")
  }
}
