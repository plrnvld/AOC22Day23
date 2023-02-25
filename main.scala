import scala.collection.mutable.HashMap
import scala.io.Source

case class Pos(val x: Int, val y: Int)

object Directions extends Enumeration {
  type Direction = Value
  val North, South, East, West = Value
}

object Main {
  def main(args: Array[String]): Unit = {
    val elvesMap = new HashMap[Pos, Directions.Direction]()
    val source = Source.fromFile("ExampleSmall.txt")

    var y = 0
    for (line <- source.getLines()) {
        println(s"$line")
        for((c, x) <- line.zipWithIndex) {
            if (c == '#') {
                elvesMap  += (Pos(x, y) -> Directions.North)
            }
        }
        y += 1
    }

    for (key <- elvesMap.keys) {
        println(key)
    }
  }
}

