import scala.util.matching.Regex
import scala.collection.Iterator
import scala.util.matching.Regex.Match

val row_forward  = "XMAS".r
val row_backward = "SAMX".r

def part1(arr: Array[String]): Unit = {
  var count = 0
  arr.foreach { row =>
    count += row_forward.findAllMatchIn(row).count(_ => true)
    count += row_backward.findAllMatchIn(row).count(_ => true)
  }
  val transposed = arr.map(_.toCharArray).transpose.map(_.mkString)
  transposed.foreach { row =>
    count += row_forward.findAllMatchIn(row).count(_ => true)
    count += row_backward.findAllMatchIn(row).count(_ => true)
  }
  for (i <- 0 until arr.length) {
    for (j <- 0 until arr(0).length) {
      if (arr(i)(j) == 'X') {
        if (searchDiag(arr, i + 1, j + 1, 1, 1, 'M')) { count += 1 }
        if (searchDiag(arr, i + 1, j - 1, 1, -1, 'M')) { count += 1 }
        if (searchDiag(arr, i - 1, j + 1, -1, 1, 'M')) { count += 1 }
        if (searchDiag(arr, i - 1, j - 1, -1, -1, 'M')) { count += 1 }
      }
    }
  }
  println(count)
}

def searchDiag(arr: Array[String], i: Int, j: Int, i2: Int, j2: Int, c: Char): Boolean = {
  if (i >= arr.length || j >= arr(0).length || j < 0 || i < 0) { return false }
  if (arr(i)(j) == c && c == 'M') { return searchDiag(arr, i + i2, j + j2, i2, j2, 'A') }
  if (arr(i)(j) == c && c == 'A') { return searchDiag(arr, i + i2, j + j2, i2, j2, 'S') }
  if (arr(i)(j) == c && c == 'S') { return true }
  false
}

def part2(arr: Array[String]): Unit = {
  var count = 0
  for {
    i <- 1 until arr.length - 1
    j <- 1 until arr(0).length - 1
    if arr(i)(j) == 'A'
    if (arr(i - 1)(j - 1) == 'M' && arr(i + 1)(j + 1) == 'S') || (arr(i - 1)(j - 1) == 'S' && arr(i + 1)(j + 1) == 'M')
    if (arr(i - 1)(j + 1) == 'M' && arr(i + 1)(j - 1) == 'S') || (arr(i - 1)(j + 1) == 'S' && arr(i + 1)(j - 1) == 'M')
  } count += 1
  println(count)
}

@main
def main(): Unit = {
  val text = io.Source.fromFile("input4.txt").mkString.split("\n")
  part1(text)
  part2(text)
}
