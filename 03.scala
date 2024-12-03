import scala.util.matching.Regex
import scala.collection.Iterator
import scala.util.matching.Regex.Match

val mul: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
val rep: Regex = """don't\(\)(.*?)(do\(\))""".r

def part1(s: String): Unit = { println(solve(mul.findAllMatchIn(s))) }

def part2(s: String): Unit = { println(solve(mul.findAllMatchIn(s.replaceAll(rep.regex, "")))) }

def solve(x: Iterator[Match]): Int = { x.map { m => m.group(1).toInt * m.group(2).toInt }.sum }

@main
def main(): Unit = {
  val text = io.Source.fromFile("input3.txt").mkString.replaceAll("\n", "")
  part1(text)
  part2(text)
}
