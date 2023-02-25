import scala.collection.mutable.HashMap
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val elvesMap = new HashMap[(Int, Int), Boolean]()
    val source = Source.fromFile("ExampleSmall.txt")

    var y = 0
    for (line <- source.getLines()) {
        println(s"$line")
        for((c, x) <- line.zipWithIndex) {
            if (c == '#') {
                elvesMap  += ((x, y) -> true)
            }
        }
        y += 1
    }

    for (key <- elvesMap.keys) {
        println(key)
    }
  }
}
