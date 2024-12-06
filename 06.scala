import scala.collection.mutable.HashMap as Map
val regex = """\^|<|v|>""".r

def parse(fileName: String): (Array[String], Int, Int, Char) = {
  val lines       = scala.io.Source.fromFile(fileName).mkString.split("\n")
  var (x, y, dir) = (0, 0, ' ')

  lines.foreach(line => {
    val m = regex.findAllIn(line).toList
    if (m.length > 0) { dir = m(0)(0); x = line.indexOf(m(0)); y = lines.indexOf(line) }
  })

  (lines, x, y, dir)
}

def part1(_lines: Array[String], _x: Int, _y: Int, _dir: Char): Unit = { println(move(_lines, _x, _y, _dir)(0)) }

def part2(_lines: Array[String], _x: Int, _y: Int, _dir: Char): Unit = {
  var (_, path) = move(_lines, _x, _y, _dir)
  println((for { line <- path; i <- line.indices; if line(i) == 'X' } yield {
    val newInput = path.updated(path.indexOf(line), line.updated(i, '#'))
    if (move(newInput, _x, _y, _dir)(0) == -1) 1 else 0
  }).sum)
}

def move(_lines: Array[String], _x: Int, _y: Int, _dir: Char): (Int, Array[String]) = {
  var (lines, x, y, dir, map) = (_lines, _x, _y, _dir, Map[(Int, Int), Int]().withDefaultValue(0))
  val (height, length)        = (lines.length, lines(0).length)
  while (x >= 0 && x < length && y >= 0 && y < height) {
    map((x, y)) += 1
    if (map((x, y)) == 5) { return (-1, lines) }
    if (lines(y)(x) != '#') {
      lines = lines.updated(y, lines(y).updated(x, 'X'))
      dir match {
        case '>' => x += 1
        case '^' => y -= 1
        case 'v' => y += 1
        case '<' => x -= 1
      }
    } else {
      dir match {
        case '>' => x -= 1; dir = 'v'
        case '^' => y += 1; dir = '>'
        case 'v' => y -= 1; dir = '<'
        case '<' => x += 1; dir = '^'
      }
    }
  }

  return (lines.map(row => row.count(_ == 'X')).sum, lines)
}

@main
def main(): Unit = {
  var (lines, x, y, dir) = parse("input6.txt")
  part1(lines, x, y, dir)
  part2(lines, x, y, dir)
}
