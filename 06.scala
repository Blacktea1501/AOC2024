import scala.collection.mutable.HashMap as Map
val regex = """\^|<|v|>""".r

def parse(fileName: String): (Array[String], Int, Int, Char) = {
  val lines = scala.io.Source.fromFile(fileName).mkString.split("\n")
  var (x, y, dir) = (0, 0, ' ')

  lines.foreach(line => {
    val m = regex.findAllIn(line).toList
    if (m.length > 0) {
      dir = m(0)(0)
      x = line.indexOf(m(0))
      y = lines.indexOf(line)
    }
  })

  (lines, x, y, dir)
}

def part1(_lines: Array[String], _x: Int, _y: Int, _dir: Char): Unit = { println(move(_lines, _x, _y, _dir)(0)) }

def part2(_lines: Array[String], _x: Int, _y: Int, _dir: Char): Unit = {
  var (_, path) = move(_lines, _x, _y, _dir)
  var count = 0
  path.foreach(line => {
    line.indices.foreach(i => {
      if (line(i) == 'X') {
        val newInput = path.updated(path.indexOf(line), line.updated(i, '#')) 
        if (move(newInput, _x, _y, _dir)(0) == -1) { count += 1 }
      }
    })
  })
  println(count)
}

def move(_lines: Array[String], _x: Int, _y: Int, _dir: Char): (Int, Array[String]) = {
  var (lines, x, y, dir) = (_lines, _x, _y, _dir)
  var map   = Map[(Int, Int), Int]().withDefaultValue(0)
  val (height, length) = (lines.length, lines(0).length)
  while (x >= 0 && x < length && y >= 0 && y < height) {
    map((x, y)) += 1
    if (map((x, y)) == 5) { return (-1, lines) }
    if (lines(y)(x) != '#') { lines = lines.updated(y, lines(y).updated(x, 'X')) }
    if (lines(y)(x) == '#') {
      dir match {
        case '>' => x -= 1; dir = 'v'
        case '^' => y += 1; dir = '>'
        case 'v' => y -= 1; dir = '<'
        case '<' => x += 1; dir = '^'
      }
    } else {
      dir match {
        case '^' => y -= 1
        case '>' => x += 1
        case 'v' => y += 1
        case '<' => x -= 1
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
