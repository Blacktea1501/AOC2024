def parse(filename: String): (List[Int], List[Int]) = {
  val text           = scala.io.Source.fromFile(filename).mkString.split("\n").map(_.split("   "))
  val (listL, listR) = text.map { case Array(l, r) => (l.toInt, r.toInt) }.unzip
  (listL.toList.sorted, listR.toList.sorted)
}

def part1(listL: List[Int], listR: List[Int]) = { println(listL.zip(listR).map((l, r) => Math.abs(l - r)).sum) }

def part2(listL: List[Int], listR: List[Int]) = { println(listL.zip(listR).map((l, r) => l * listR.count(_ == l)).sum) }

@main
def main(): Unit = {
  val (listL, listR) = parse("input.txt")
  part1(listL, listR)
  part2(listL, listR)
}
