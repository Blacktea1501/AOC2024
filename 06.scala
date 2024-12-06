class Maze(l: List[String]) {

  private var maze = l

  def move(xx: Int, yy: Int, dirr: Char): Int = {
    var (x, y, dir) = (xx, yy, dirr)
    val height      = maze.length
    val length      = maze(0).length
    var steps       = 0

    while (x >= 0 && x < length && y >= 0 && y < height) {
      if (steps > (height * length) * 0.35) { return -1 }
      if (maze(y)(x) != '#') { maze = maze.updated(y, maze(y).updated(x, 'X')) }

      if (maze(y)(x) == '#') {
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
      // code for terminal visualization
      // maze.foreach(println)
      // Thread.sleep(100)
      // // clear screen
      // print("\u001b[H\u001b[2J")
      steps += 1
    }
    return maze.map(row => row.count(_ == 'X')).sum
  }

}

def parse(): (List[String], Int, Int, Char) = {
  var input = scala.io.Source.fromFile("input6.txt").getLines().toList
  var x     = 0
  var y     = 0
  var dir   = ' '
  input.foreach(line => {
    val m = regex.findAllIn(line).toList
    if (m.length > 0) {
      dir = m(0)(0)
      x = line.indexOf(m(0))
      y = input.indexOf(line)
    }
  })
  (input, x, y, dir)
}

val regex = """\^|<|v|>""".r

def part1(input: List[String], x: Int, y: Int, dir: Char): Unit = {
  val maze = new Maze(input)
  println(maze.move(x, y, dir))
}

def part2(input: List[String], x: Int, y: Int, dir: Char): Unit = {
  var count = 0
  input.foreach(line => {
    line.indices.foreach(i => {
      if (line(i) == '.') {
        val newInput = input.updated(input.indexOf(line), line.updated(i, '#'))
        val maze     = new Maze(newInput)
        val res      = maze.move(x, y, dir)
        if (res == -1) { count += 1 }
      }
    })
  })
  println(count)
}

@main
def main(): Unit = {
  var (input, x, y, dir) = parse()
  part1(input, x, y, dir)
  part2(input, x, y, dir)
}
