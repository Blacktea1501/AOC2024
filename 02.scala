def allIncreasing(arr: Array[Int]): Boolean = {
  arr.sliding(2).forall { case Array(a, b) => a < b }
}

def allDecreasing(arr: Array[Int]): Boolean = {
  arr.sliding(2).forall { case Array(a, b) => a > b }
}

def allDifferencesInRange(arr: Array[Int], min: Int, max: Int): Boolean = {
  arr.sliding(2).forall { case Array(a, b) => (a - b).abs >= min && (a - b).abs <= max }
}

def tryRemoveLevel(report: Array[Int]): Boolean = {
  report.indices.exists { i => check(report.take(i) ++ report.drop(i + 1)) }
}

def check(report: Array[Int]): Boolean = {
  (allIncreasing(report) || allDecreasing(report)) && allDifferencesInRange(report, 1, 3)
}

def parse(filename: String): Array[Array[Int]] = {
  val input = scala.io.Source.fromFile(filename).mkString
  input.split("\n").map(line => line.split(" ").map(_.toInt))
}

def part1(reports: Array[Array[Int]]): Unit = {
  var safeReports = 0
  reports.foreach { report => if (check(report)) { safeReports += 1 } }
  println(s"Safe reports: $safeReports")
}

def part2(reports: Array[Array[Int]]): Unit = {
  var safeReports = 0
  reports.foreach { report => if (check(report) || tryRemoveLevel(report)) { safeReports += 1 } }
  println(s"Safe reports: $safeReports")
}

@main
def main(): Unit = {
  val reports = parse("input2.txt")
  part1(reports)
  part2(reports)
}
