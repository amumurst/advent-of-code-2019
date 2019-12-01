package no.amumurst

object Main extends App {
  args.headOption match {
    case Some("1")  => Day1.run
    case Some("1b") => Day1Part2.run
    case _          => println("No day selected")
  }
}
