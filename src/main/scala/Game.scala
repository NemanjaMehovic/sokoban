import java.io.{BufferedWriter, FileWriter}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.{Try, Using}

class Game(val map:List[List[Zone]], val heroX:Int, val heroY:Int) {

  private def switch(newMap: Array[Array[Zone]], x1: Int, y1: Int, f: (Int, Int) => (Int, Int), depth: Int): Boolean = {
    val tuple = f(x1, y1)
    if (tuple._2 >= newMap.length || tuple._2 < 0 || tuple._1 >= newMap.head.length || tuple._1 < 0 || depth > 1)
      return false
    if (newMap(tuple._2)(tuple._1).canMoveTo) {
      newMap(tuple._2)(tuple._1) = newMap(tuple._2)(tuple._1).moveTo(newMap(y1)(x1))
      newMap(y1)(x1) = newMap(y1)(x1).moveFrom
      return true
    }
    else if (newMap(tuple._2)(tuple._1).isMovable) {
      if (switch(newMap, tuple._1, tuple._2, f, depth + 1)) {
        return switch(newMap, x1, y1, f, depth + 1)
      }
    }
    false
  }

  def move(direction: Char): (Game, Boolean) = {
    val newMap: Array[Array[Zone]] = Array.ofDim[Zone](map.length, map.head.length)
    for(i <- map.indices){
      for(j <- map.head.indices){
        newMap(i)(j) = map(i)(j)
      }
    }
    var f:(Int, Int) => (Int, Int) = null
    direction match {
      case 'U' =>
        f = (x, y) => { (x, y - 1)}
      case 'D' =>
        f = (x, y) => { (x, y + 1) }
      case 'L' =>
        f = (x, y) => { (x - 1, y) }
      case 'R' =>
        f = (x, y) => { (x + 1, y) }
    }
    if(switch(newMap, heroX, heroY, f, 0)){
      var list = List[List[Zone]]()
      for(array <- newMap){
        list = list.appended(array.toList)
      }
      val tuple = f(heroX, heroY)
      (new Game(list, tuple._1, tuple._2), true)
    }
    else
      (this, false)
  }

  def isDone() = {
    var numOfBoxes = 0
    for (list <- map) {
      for (zone <- list) {
        if (zone.isInstanceOf[Box])
          numOfBoxes += 1
      }
    }
    numOfBoxes == 0
  }

  @tailrec
  private def flood(positions:Queue[(Int, Int)], history:List[(Int,Int)], numOfBoxesFinishes:(Int, Int)): (Boolean, (Int, Int)) = {
    if(positions.isEmpty)
      return (true, numOfBoxesFinishes)
    val positionQueueTuple = positions.dequeue
    val x = positionQueueTuple._1._1
    val y = positionQueueTuple._1._2
    val width = map.head.length
    val height = map.length
    if(x < 0 || x >= width || y < 0 || y >= height) {
      println(x + " " + y)
      return (false, numOfBoxesFinishes)
    }
    if(map(y)(x).isWall || history.contains((x,y)))
      flood(positionQueueTuple._2, history, numOfBoxesFinishes)
    else {
      val newQueue = positionQueueTuple._2.enqueueAll(Queue[(Int, Int)]((x, y + 1),(x, y - 1),(x + 1, y),(x - 1, y)))
      val newHistory = history.appended((x, y))
      map(y)(x) match {
        case _:Box => flood(newQueue, newHistory, (numOfBoxesFinishes._1 + 1, numOfBoxesFinishes._2))
        case _:Finish => flood(newQueue, newHistory, (numOfBoxesFinishes._1, numOfBoxesFinishes._2 + 1))
        case _: BoxOnFinish => flood(newQueue, newHistory, (numOfBoxesFinishes._1 + 1, numOfBoxesFinishes._2 + 1))
        case _ => flood(newQueue, newHistory, numOfBoxesFinishes)
      }
    }
  }

  def isValidMap() = {
    assert(heroX != -1 && heroY != -1,"No hero on map")
    var numOfBoxes = 0
    var numOfFinishes = 0
    var numOfHeroes = 0
    var len = -1
    for(row <- map){
      for(zone <- row){
        zone match {
          case _: Box => numOfBoxes += 1
          case _: Finish => numOfFinishes += 1
          case _: Hero => numOfHeroes += 1
          case _: BoxOnFinish =>
            numOfBoxes += 1
            numOfFinishes += 1
          case _ => //do nothing
        }
      }
      assert(len == -1 || len == row.length, "Map rows are not the same length")
      len = row.length
    }
    assert(numOfHeroes == 1, "There can only be one hero on a map")
    assert(numOfBoxes == numOfFinishes, "There is not the same number of boxes and finishes")
    val floodResult = flood(Queue[(Int, Int)]((heroX, heroY)), List[(Int, Int)](), (0, 0))
    assert(floodResult._1, "Not a closed space")
    assert(floodResult._2._1 == numOfBoxes, "Not all boxes found inside closed play area")
    assert(floodResult._2._2 == numOfFinishes, "Not all finishes found inside closed play area")
    true
  }
}

object Game{

  private def createZone(ch: Char): Option[Zone] = ch.toLower match {
    case '-' => Some(new Tile)
    case '–' => Some(new Tile)
    case '#' => Some(new Wall)
    case 's' => Some(new Hero)
    case '.' => Some(new Finish)
    case 'x' => Some(new Box)
    case 'o' => Some(new BoxOnFinish)
    case _ => None
  }

  def createGame(fileName: String): Try[Game]= {
    Using.Manager { use =>
      val bufferedSource = use(Source.fromFile(fileName))

      var list: List[List[Zone]] = List[List[Zone]]()
      var heroX = -1
      var heroY = -1
      var i = 0
      for (line <- bufferedSource.getLines) {
        var tmpList: List[Zone] = List[Zone]()
        var j = 0
        for (ch <- line) {
          val zone = createZone(ch)
          assert(zone.isDefined, "Symbol \'" + ch + "\' is not defined in maps")
          tmpList = tmpList.appended(zone.get)
          if (zone.get.isHero) {
            heroX = j
            heroY = i
          }
          j += 1
        }
        list = list.appended(tmpList)
        i += 1
      }
      new Game(list, heroX, heroY)
    }
  }

  private def badCharCheck(ch: Char): Boolean = {
    ch match {
      case 'U' => false
      case 'D' => false
      case 'L' => false
      case 'R' => false
      case _ => true
    }
  }

  def getMovesFromFile(fileName: String): Try[List[Char]] = {
    Using.Manager{ use =>
      val bufferedSource = use(Source.fromFile(fileName))
      var lines = List[String]()
      for(line <- bufferedSource.getLines){
        lines = lines.appended(line)
      }
      assert(!lines.exists(str => str.length > 1 || badCharCheck(str.charAt(0))), "Bad file format")
      lines.map[Char](str => str.charAt(0))
    }
  }

  def saveMovesToFile(fileName: String, moves: List[Char]): Try[Boolean] = {
    Using.Manager{use =>
     val bufferedWriter = use(new BufferedWriter(new FileWriter(fileName)))
      moves.foreach(ch => {
        bufferedWriter.write(ch)
        bufferedWriter.newLine()
      })
      true
    }
  }

  def saveGame(game: Game): Try[Boolean] = {
    Using.Manager { use =>
      game.isValidMap()
      val bufferedWriter = use(new BufferedWriter(new FileWriter("gameMap.txt")))
      game.map.foreach(list => {
        list.foreach(zone =>{
          bufferedWriter.write(zone.toString)
        })
        bufferedWriter.newLine()
      })
      true
    }
  }
}
