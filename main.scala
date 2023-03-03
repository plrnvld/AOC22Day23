import scala.collection.mutable.HashMap
import scala.io.Source

case class Pos(val x: Int, val y: Int) {
    def move(dir: Directions.Direction): Pos = {
        dir match {
            case Directions.North => Pos(this.x, this.y-1)
            case Directions.South => Pos(this.x, this.y+1)
            case Directions.West => Pos(this.x-1, this.y)
            case Directions.East => Pos(this.x+1, this.y)
        }
    }
}

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

            // Collect proposals
            for ((pos, nextDir) <- currentPositions) {
                // Check all 8 positions
                val doNothing = noElvesAround(pos, currentPositions)
                val proposal: Pos = if (doNothing) {
                    pos
                } else {
                    val nextDir = currentPositions(pos)
                    val allToCheck = toCheck(nextDir)
                    val open = allToCheck.find(d => canMove(pos, d, currentPositions))

                    open match {
                        case Some(dir) => pos.move(dir)
                        case None => pos
                    }               
                }

                // Insert proposal
                if (proposalMap.contains(proposal)) {
                    val allPossesProposing = proposalMap(proposal)
                    proposalMap(proposal) = allPossesProposing :+ pos
                } else {
                    proposalMap(proposal) = List(pos)
                }                
            }

            // Update positions
            for ((proposal, posses) <- proposalMap) {
                if (posses.length == 1) {
                    val pos = posses.head
                    val nextDir = currentPositions(pos)
                    currentPositions -= pos
                    currentPositions(pos) = nextDir
                } else {
                   // Do nothing   
                }                
            }            

            // Switch nextDir to check
            for ((pos, nextDir) <- currentPositions) {
                currentPositions(pos) = nextDirection(nextDir)
            }
        }

        positions
    }    

    def canMove(pos: Pos, dir: Directions.Direction, currentPositions: HashMap[Pos, Directions.Direction]): Boolean = {
        val cp = currentPositions
        val x = pos.x
        val y = pos.y
        dir match {
            case Directions.North => emp(x-1, y-1, cp) && emp(x, y-1, cp) && emp(x+1, y-1, cp)
            case Directions.South => emp(x-1, y+1, cp) && emp(x, y+1, cp) && emp(x+1, y+1, cp)
            case Directions.West => emp(x-1, y+1, cp) && emp(x-1, y, cp) && emp(x-1, y-1, cp)
            case Directions.East => emp(x+1, y+1, cp) && emp(x+1, y, cp) && emp(x+1, y-1, cp)
        }
    }

    def noElvesAround(pos: Pos, currentPositions: HashMap[Pos, Directions.Direction]): Boolean = {
        val x = pos.x
        val y = pos.y
        val cp = currentPositions
        
        emp(x-1, y-1, cp) && emp(x, y-1, cp) && emp(x+1, y-1, cp) && emp(x-1, y, cp) && emp(x+1, y, cp) && emp(x-1, y+1, cp) && emp(x, y+1, cp) && emp(x+1, y+1, cp)
    }

    def emp(x: Int, y: Int, currentPositions: HashMap[Pos, Directions.Direction]): Boolean = 
        !currentPositions.contains(Pos(x, y))

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

