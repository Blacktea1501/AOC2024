def parse(filename: String): (List[Int], List[Int]) = {
  val text = scala.io.Source.fromFile(filename).mkString
  val (listL, listR) = text.split("\n").map(_.split("   ")).map { case Array(l, r) => (l.toInt, r.toInt) }.unzip
  (listL.toList.sorted, listR.toList.sorted)
}

def part1(listL: List[Int], listR: List[Int]) = {
  var sum = 0
  listL.foreach(l => sum += Math.abs(l - listR(listL.indexOf(l))))
  println(sum)
}

def part2(listL: List[Int], listR: List[Int]) = {
  var sum = 0
  listL.foreach { l => sum += l * listR.count(_ == l) }
  println(sum)
}

@main
def main(): Unit = {
  val (listL, listR) = parse("input.txt")
  part1(listL, listR)
  part2(listL, listR)
}
