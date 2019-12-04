package no.amumurst

object Main extends App {
  args.headOption match {
    case Some("1")  => Day1.run
    case Some("1b") => Day1Part2.run
    case Some("2")  => Day2.run
    case Some("2b") => Day2Part2.run
    case Some("3")  => Day3.run
    case Some("3b") => Day3Part2.run
    case Some("4")  => Day4.run
    case Some("4b") => Day4Part2.run
    case _          => println("No day selected")
  }
}
