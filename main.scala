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

    def toCheck(dir: Directions.Direction): List[Directions.Direction] = {
        dir match {
            case Directions.North => List(Directions.North, Directions.South, Directions.West, Directions.East)
            case Directions.South => List(Directions.South, Directions.West, Directions.East, Directions.North)
            case Directions.West => List(Directions.West, Directions.East, Directions.North, Directions.South)
            case Directions.East => List(Directions.East, Directions.North, Directions.South, Directions.West)
        }
    }

    def positionsFacing(pos: Pos, dir: Directions.Direction): List[Pos] = {
        val x = pos.x;
        val y = pos.y;

        dir match {
            case Directions.North => List(Pos(x-1, y-1), Pos(x, y-1), Pos(x+1, y-1))
            case Directions.South => List(Pos(x+1, y+1), Pos(x, y+1), Pos(x-1, y+1))
            case Directions.West => List(Pos(x-1, y+1), Pos(x-1, y), Pos(x-1, y-1))
            case Directions.East => List(Pos(x+1, y+1), Pos(x+1, y), Pos(x+1, y))
        }
    }
}

