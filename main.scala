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

    val navigator = new ElfNavigator(elvesMap)
    navigator.predictRounds(3)
  }
}


class ElfNavigator(val initial: HashMap[Pos, Directions.Direction]) {

    def predictRounds(num: Int): List[Pos] = {
        val positions = List[Pos]()

        var currentPositions = initial.clone()        
        
        for (t <- 1 to num) {
            println(s"> round $t")

            val proposalMap = new HashMap[Pos, List[Pos]]

            for ((pos, nextDir) <- currentPositions)
                println("###")
        }

        positions
    }

    def nextDirection(dir: Directions.Direction): Directions.Direction = {
        dir match {
            case Directions.North => Directions.South
            case Directions.South => Directions.West
            case Directions.West => Directions.East
            case Directions.East => Directions.North
        }
    }
}

