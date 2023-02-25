import scala.collection.mutable.HashMap
import scala.io.Source


object Directions extends Enumeration {
  type Direction = Value
  val North, South, East, West = Value
}

object Main {
  def main(args: Array[String]): Unit = {
    val elvesMap = new HashMap[(Int, Int), Directions.Direction]()
    val source = Source.fromFile("ExampleSmall.txt")

    var y = 0
    for (line <- source.getLines()) {
        println(s"$line")
        for((c, x) <- line.zipWithIndex) {
            if (c == '#') {
                elvesMap  += ((x, y) -> Directions.North)
            }
        }
        y += 1
    }

    for (key <- elvesMap.keys) {
        println(key)
    }
  }
}

