package no.amumurst

object Day10 {
  def delta(x1: Int, x2: Int, y1: Int, y2: Int): Double = -math.atan2(x2 - x1, y2 - y1)

  def pointsOfType(ls: LazyList[LazyList[Char]], C: Char): Set[(Int, Int)] =
    ls.zipWithIndex.flatMap {
      case (l, y) => l.zipWithIndex.collect { case (C, x) => (x, y) }
    }.toSet

  def distLines(ls: LazyList[LazyList[Char]]) = {
    val points: Set[(Int, Int)] = pointsOfType(ls, '#')
    points.map { case a @ (x, y) => (points.filterNot(_ == a).map { case (x2, y2) => delta(x, x2, y, y2) }.size, a) }.max
  }

  def testParser(s: String) = s.split('\n').map(_.to(LazyList)).to(LazyList)
  lazy val run = {

    val test1 = ".#..#\n.....\n#####\n....#\n...##"
    distLines(testParser(test1)).printAssert(8, (3, 4))

    val test2 =
      "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
    distLines(testParser(test2)).printAssert(33, (5, 8))

    val test3 =
      "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
    distLines(testParser(test3)).printAssert(35, (1, 2))

    val test4 =
      ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
    distLines(testParser(test4)).printAssert(41, (6, 3))

    val test5 =
      ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
    distLines(testParser(test5)).printAssert(210, (11, 13))

    val input = readFileLines("Day10.txt").map(_.to(LazyList))
    distLines(input).printTimed
  }
}

object Day10Part2 {

  def distance(x1: Int, x2: Int, y1: Int, y2: Int): Int = (x2 - x1).abs + (y2 - y1).abs

  def program(ls: LazyList[LazyList[Char]]) = {
    val (x1, y1) = Day10.pointsOfType(ls, 'X').head

    def loop(as: List[(Int, Int)], dest: List[List[(Int, Int)]]): List[List[(Int, Int)]] =
      as.groupBy { case (x2, y2) => Day10.delta(x1, x2, y1, y2) }
        .view
        .mapValues(_.minBy { case (x2, y2) => distance(x1, x2, y1, y2) })
        .toList
        .sortBy(_._1)
        .map(_._2) match {
        case Nil => dest
        case ret => loop(as.diff(ret), dest :+ ret)
      }

    loop(Day10.pointsOfType(ls, '#').toList, Nil).flatten
  }

  lazy val run = {
    val test = Day10.testParser(
      ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##"
    )
    program(test).last.printAssert((14, 3))

    val bigtest =
      program(Day10.testParser(
        ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.####X#####...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"))

    bigtest.drop(1 - 1).head.printAssert((11, 12))
    bigtest.drop(2 - 1).head.printAssert((12, 1))
    bigtest.drop(3 - 1).head.printAssert((12, 2))
    bigtest.drop(10 - 1).head.printAssert((12, 8))
    bigtest.drop(20 - 1).head.printAssert((16, 0))
    bigtest.drop(50 - 1).head.printAssert((16, 9))
    bigtest.drop(100 - 1).head.printAssert((10, 16))
    bigtest.drop(199 - 1).head.printAssert((9, 6))
    bigtest.drop(200 - 1).head.printAssert((8, 2))
    bigtest.drop(201 - 1).head.printAssert((10, 9))
    bigtest.drop(299 - 1).head.printAssert((11, 1))

    {
      val file        = readFileLines("Day10.txt").map(_.to(LazyList))
      val (x, y)      = Day10.distLines(file)._2
      val updatedFile = file.updated(y, file(y).updated(x, 'X'))
      val (lx, ly)    = program(updatedFile).drop(199).head
      lx * 100 + ly
    }.printTimed
  }
}
