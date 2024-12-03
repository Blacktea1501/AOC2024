def allOrdered(arr: Array[Int], comp: (Int, Int) => Boolean): Boolean = {
  arr.sliding(2).forall { case Array(a, b) => comp(a, b) }
}

def allDifferencesInRange(arr: Array[Int]): Boolean = {
  arr.sliding(2).forall { case Array(a, b) => (a - b).abs >= 1 && (a - b).abs <= 3 }
}

def tryRemoveLevel(report: Array[Int]): Boolean = {
  report.indices.exists(i => check(report.take(i) ++ report.drop(i + 1)))
}

def check(report: Array[Int]): Boolean = {
  (allOrdered(report, _ < _) || allOrdered(report, _ > _)) && allDifferencesInRange(report)
}

def part1(reports: Array[Array[Int]]): Unit = { println(reports.count(check)) }

def part2(reports: Array[Array[Int]]): Unit = {
  println(reports.foldLeft(0)((acc, report) => if (check(report) || tryRemoveLevel(report)) acc + 1 else acc))
}

@main
def main(): Unit = {
  val reports = scala.io.Source.fromFile("input2.txt").mkString.split("\n").map(line => line.split(" ").map(_.toInt))
  part1(reports)
  part2(reports)
}
