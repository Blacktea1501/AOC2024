import scala.collection.mutable.HashMap

def parse(filename: String): (HashMap[Int, List[Int]], List[List[Int]]) = {
  val lines = scala.io.Source.fromFile(filename).mkString.split("\n").filter(!_.isEmpty())

  var rules   = HashMap[Int, List[Int]]()
  var updates = List[List[Int]]()

  val tuples = lines.filter(_.matches("^\\d+\\|\\d+$"))
    .map(_.split("\\|").map(_.trim().toInt))
    .map { case Array(x, y) => (x, y) }
    .toList

  tuples.foreach { case (x, y) => rules(x) = rules.getOrElse(x, List()) :+ y }

  updates = lines.filter(_.matches("^[\\d+,]*$")).map(_.split(",").map(_.trim().toInt).toList).toList

  (rules, updates)
}

def isValid(rules: HashMap[Int, List[Int]], update: List[Int]): Boolean = {
  (1 until update.length).forall(i => !rules.contains(update(i)) || !rules(update(i)).contains(update(i - 1)))
}

def sort(rules: HashMap[Int, List[Int]], update: List[Int]): List[Int] = {
  update.sortBy(x => rules.getOrElse(x, List()).map(y => update.count(_ == y)).sum)
}

def part1(rules: HashMap[Int, List[Int]], updates: List[List[Int]]): Unit = {
  println(updates.filter(update => isValid(rules, update)).map(update => update(update.length / 2)).sum)
}

def part2(rules: HashMap[Int, List[Int]], updates: List[List[Int]]): Unit = {
  println(updates.filter(update => !isValid(rules, update)).map(update => sort(rules, update)(update.length / 2)).sum)
}

@main
def main(): Unit = {
  val (rules, updates) = parse("input5.txt")
  part1(rules, updates)
  part2(rules, updates)
}
