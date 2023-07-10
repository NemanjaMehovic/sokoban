import scala.annotation.tailrec
import scala.collection.immutable.{Queue, Set}

object GameEditor {

  def addRow(beginning: Boolean, game: Game): Game = {
    val len = game.map.head.length
    val fillList:List[Zone] = List.fill(len)(new Tile)
    if(beginning)
      new Game(List[List[Zone]](fillList):::game.map, game.heroX, game.heroY + 1)
    else
      new Game(game.map.appended(fillList), game.heroX, game.heroY)
  }

  def addColumn(beginning: Boolean, game: Game): Game = {
    var newMap: List[List[Zone]] = List[List[Zone]]()
    for(row <- game.map){
      if(beginning)
        newMap = newMap.appended(List(new Tile):::row)
      else
        newMap = newMap.appended(row.appended(new Tile))
    }
    if (beginning)
      new Game(newMap, game.heroX + 1, game.heroY)
    else
      new Game(newMap, game.heroX, game.heroY)
  }

  def removeRow(beginning: Boolean, game: Game): Game = {
    if(game.map.length <= 1)
      return game
    if (beginning)
      new Game(game.map.drop(1), game.heroX, game.heroY - 1)
    else
      new Game(game.map.dropRight(1), game.heroX, game.heroY)
  }

  def removeColumn(beginning: Boolean, game: Game): Game = {
    if(game.map.head.length <= 1)
      return game
    var newMap:List[List[Zone]] = List[List[Zone]]()
    for (row <- game.map) {
      if (beginning)
        newMap = newMap.appended(row.drop(1))
      else
        newMap = newMap.appended(row.dropRight(1))
    }

    if (beginning)
      new Game(newMap, game.heroX - 1, game.heroY)
    else
      new Game(newMap, game.heroX, game.heroY)
  }

  def swapZone(x: Int, y: Int, newZone: Zone, game: Game): Game = {
    val newMap = game.map.updated(y, game.map(y).updated(x, newZone))
    if(newZone.isHero)
      new Game(newMap, x, y)
    else
      new Game(newMap, game.heroX, game.heroY)
  }

  def inverse(game: Game): Game = {
    var newMap: List[List[Zone]] = List[List[Zone]]()
    for(row <- game.map){
      var newList: List[Zone] = List[Zone]()
      for(zone <- row){
        zone match {
          case _: Box => newList = newList.appended(new Finish)
          case _: Finish => newList = newList.appended(new Box)
          case _ => newList = newList.appended(zone)
        }
      }
      newMap = newMap.appended(newList)
    }
    new Game(newMap, game.heroX, game.heroY)
  }

  @tailrec
  private def removeWalls(cord:(Int, Int), game:Game, set:Set[(Int, Int)]): Game = {
    if(cord._2 == game.map.length)
      game
    else{
      if(cord._1 == game.map.head.length) {
        removeWalls((0, cord._2 + 1), game, set)
      }
      else{
        if(game.map(cord._2)(cord._1).isWall  && !set.exists( tuple => cord._1 == tuple._1 && cord._2 == tuple._2))
          removeWalls((cord._1 + 1, cord._2), swapZone(cord._1, cord._2, new Tile, game), set)
        else
          removeWalls((cord._1 + 1, cord._2), game, set)
      }
    }
  }

  @tailrec
  def minimise(positions: Queue[(Int, Int)], history: List[(Int, Int)], game:Game, set:Set[(Int, Int)]): Game = {
    if (positions.isEmpty) {
      return removeWalls((0, 0), game, set)
    }
    val positionQueueTuple = positions.dequeue
    val x = positionQueueTuple._1._1
    val y = positionQueueTuple._1._2
    val width = game.map.head.length
    val height = game.map.length
    if (x < 0 || x >= width || y < 0 || y >= height)
      minimise(positionQueueTuple._2, history, game, set)
    else {
      if (game.map(y)(x).isWall || history.contains((x, y))) {
        if (game.map(y)(x).isWall) {
          val tuple = (x, y)
          minimise(positionQueueTuple._2, history, game, set + tuple)
        } else
          minimise(positionQueueTuple._2, history, game, set)
      }
      else {
        val newQueue = positionQueueTuple._2.enqueueAll(Queue[(Int, Int)]((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)))
        val newHistory = history.appended((x, y))
        minimise(newQueue, newHistory, game, set)
      }
    }
  }

  def filter(x:Int, y:Int, game: Game, n: Int): Game = {
    val row = game.map(y)
    val col = for( tmpRow <- game.map) yield tmpRow(x)
    if(row.indices.exists( cord => row(cord).isWall && (cord - x).abs <= n) ||
      col.indices.exists( cord => col(cord).isWall && (cord - y).abs <= n))
      swapZone(x, y, new Tile, game)
    else
      game
  }

  @tailrec
  private def checkIsInside(positions: Queue[(Int, Int)], history: List[(Int, Int)], game: Game, foundSomething: Boolean): Boolean = {
    if (positions.isEmpty) {
      return foundSomething
    }
    val positionQueueTuple = positions.dequeue
    val x = positionQueueTuple._1._1
    val y = positionQueueTuple._1._2
    val width = game.map.head.length
    val height = game.map.length
    if (x < 0 || x >= width || y < 0 || y >= height)
      false
    else {
      if (game.map(y)(x).isWall || history.contains((x, y)))
        checkIsInside(positionQueueTuple._2, history, game, foundSomething)
      else {
        val newQueue = positionQueueTuple._2.enqueueAll(Queue[(Int, Int)]((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)))
        val newHistory = history.appended((x, y))
        if(!game.map(y)(x).isInstanceOf[Tile])
          checkIsInside(newQueue, newHistory, game, true)
        else
          checkIsInside(newQueue, newHistory, game, foundSomething)
      }
    }
  }

  @tailrec
  private def getNeighbouringWalls(xy: (Int, Int), skipXY: (Int, Int), game: Game, result: List[(Int, Int)]): List[(Int, Int)] = {
    if(xy._2 > (skipXY._2 + 1))
      result
    else {
      if (xy._1 > (skipXY._1 + 1))
        getNeighbouringWalls((skipXY._1 - 1, xy._2 + 1), skipXY, game, result)
      else{
        if(game.map(xy._2)(xy._1).isWall && (xy._1 != skipXY._1 || xy._2 != skipXY._2))
          getNeighbouringWalls((xy._1 + 1, xy._2), skipXY, game, result.appended(xy))
        else
          getNeighbouringWalls((xy._1 + 1, xy._2), skipXY, game, result)
      }
    }
  }

  @tailrec
  private def expend(x: Int, y: Int, game: Game): (Game, (Int, Int)) = {
    if ((x - 1) < 0)
      expend(x + 1, y, addColumn(true, game))
    else if ((x + 1) >= game.map.head.length)
      expend(x, y, addColumn(false, game))
    else if ((y - 1) < 0)
      expend(x, y + 1, addRow(true, game))
    else if ((y + 1) >= game.map.length)
      expend(x, y, addRow(false, game))
    else
      (game, (x, y))
  }

  private def addWalls(x: Int, y: Int, game: Game, dontChange: Set[(Int, Int)]): Game = {
    val gameUp = if (dontChange.exists(tuple => tuple._1 == x && tuple._2 == (y - 1)) || !game.map(y - 1)(x).isInstanceOf[Tile]) game else swapZone(x, y - 1, new Wall, game)
    val gameDown = if (dontChange.exists(tuple => tuple._1 == x && tuple._2 == (y + 1)) || !game.map(y + 1)(x).isInstanceOf[Tile]) gameUp else swapZone(x, y + 1, new Wall, gameUp)
    val gameLeft = if (dontChange.exists(tuple => tuple._1 == (x - 1) && tuple._2 == y) || !game.map(y)(x - 1).isInstanceOf[Tile]) gameDown else swapZone(x - 1, y, new Wall, gameDown)
    val gameRight = if (dontChange.exists(tuple => tuple._1 == (x + 1) && tuple._2 == y) || !game.map(y)(x + 1).isInstanceOf[Tile]) gameLeft else swapZone(x + 1, y, new Wall, gameLeft)
    gameRight
  }

  private def checkIfRemovingWallsIsGoodEnough(x: Int, y: Int, game: Game, fakeWalls: List[(Int, Int)], constFunctions: List[(Int => Int, Int => Int)], usingFunctions: List[(Int => Int, Int => Int)]): (Game, Boolean) = {
    if (usingFunctions.isEmpty)
      return (game, false)
    val fx = usingFunctions.head._1
    val fy = usingFunctions.head._2
    if (game.map(fy(y))(fx(x)).isWall && fakeWalls.exists(tuple => tuple._1 == fx(x) && tuple._2 == fy(y))) {
      val gameNoWall = swapZone(fx(x), fy(y), new Tile, game)
      if (checkIsInside(Queue[(Int, Int)]((x, y)), List[(Int, Int)](), gameNoWall, false)) {
        val gameUpdate = checkIfRemovingWallsIsGoodEnough(x, y, gameNoWall, fakeWalls, constFunctions, constFunctions)
        if (gameUpdate._2)
          gameUpdate
        else
          (gameNoWall, true)
      }
      else
        checkIfRemovingWallsIsGoodEnough(x, y, game, fakeWalls, constFunctions, usingFunctions.drop(1))
    }
    else
      checkIfRemovingWallsIsGoodEnough(x, y, game, fakeWalls, constFunctions, usingFunctions.drop(1))
  }

  private def recCalls(x: Int, y: Int, result: (Game, Boolean), fCondition:((Int, Int), (Int, Int)) => Boolean,realWalls: List[(Int, Int)], generatedWalls: Set[(Int, Int)], dontChange: Set[(Int, Int)], usingFunctions: List[(Int => Int, Int => Int)]): (Game, Boolean) = {
    if (usingFunctions.isEmpty)
      return result
    val fx = usingFunctions.head._1
    val fy = usingFunctions.head._2
    val newX = fx(x)
    val newY = fy(y)
    if(realWalls.exists(tuple => fCondition((newX, newY), tuple))) {
      val newResult = fractal(newX, newY, result._1, generatedWalls, dontChange)
      recCalls(x, y, newResult, fCondition, realWalls, generatedWalls, dontChange, usingFunctions.drop(1))
    }
    else
      recCalls(x, y, result, fCondition, realWalls, generatedWalls, dontChange, usingFunctions.drop(1))
  }

  def fractal(x: Int, y: Int, game: Game, generatedWalls: Set[(Int, Int)], dontChange: Set[(Int, Int)]): (Game, Boolean) = {
    if(!game.map(y)(x).isWall)
      return (game, false)

    val expendedTuple = expend(x, y, game)
    val gameExpended = expendedTuple._1
    val xExpended = expendedTuple._2._1
    val yExpended = expendedTuple._2._2

    val gameRemovedWall = swapZone(xExpended, yExpended, new Tile, gameExpended)

    val neighbouringWalls = getNeighbouringWalls((xExpended - 1, yExpended - 1), (xExpended, yExpended), gameRemovedWall, List[(Int, Int)]())
    val realWalls = neighbouringWalls.filter( wall => !generatedWalls.exists( generated => generated._1 == wall._1 && generated._2 == wall._2))

    if(realWalls.isEmpty)
      return (game, false)

    val gameAddedFakeWalls = addWalls(xExpended, yExpended, gameRemovedWall, dontChange)
    //if (checkIsInside(Queue[(Int, Int)]((xExpended, yExpended)), List[(Int, Int)](), gameAddedFakeWalls, false))
      //return (gameAddedFakeWalls, true)

    val neighbouringWallsAfterAdding = getNeighbouringWalls((xExpended - 1, yExpended - 1), (xExpended, yExpended), gameAddedFakeWalls, List[(Int, Int)]())
    val fakeWalls = neighbouringWallsAfterAdding.filter( wall => realWalls.forall( real => real._1 != wall._1 || real._2 != wall._2))

    val fAdd1 = (tmp: Int) => tmp + 1
    val fSub1 = (tmp: Int) => tmp - 1
    val fNothing = (tmp: Int) => tmp
    val functionsList = List[(Int => Int, Int => Int)]((fNothing, fSub1), (fNothing, fAdd1), (fSub1, fNothing), (fAdd1, fNothing))

    val finalCheck = checkIfRemovingWallsIsGoodEnough(xExpended, yExpended, gameAddedFakeWalls, fakeWalls, functionsList, functionsList)
    if(finalCheck._2)
      return finalCheck

    //izbegava bug ako se uradi posle wall removal
    if (checkIsInside(Queue[(Int, Int)]((xExpended, yExpended)), List[(Int, Int)](), gameRemovedWall, false))
      return (gameRemovedWall, true)

    val realWallsCondition = (tuple1:(Int, Int), tuple2:(Int, Int)) => tuple1._1 == tuple2._1 && tuple1._2 == tuple2._2
    val realWallsExpand = recCalls(xExpended, yExpended, finalCheck,  realWallsCondition, realWalls, generatedWalls ++ fakeWalls, dontChange + Tuple2(xExpended, yExpended), functionsList)

    if(realWallsExpand._2)
      return realWallsExpand

    val fakeWallsCondition = (tuple1:(Int, Int), tuple2:(Int, Int)) => {
      (tuple2._1 == tuple1._1 && tuple2._2 == (tuple1._2 - 1)) ||
        (tuple2._1 == tuple1._1 && tuple2._2 == (tuple1._2 + 1)) ||
        (tuple2._1 == (tuple1._1 - 1) && tuple2._2 == tuple1._2)||
        (tuple2._1 == (tuple1._1 + 1) && tuple2._2 == tuple1._2)
    }

    val fakeWallsExpand = recCalls(xExpended, yExpended, realWallsExpand,  fakeWallsCondition, realWalls, generatedWalls ++ fakeWalls, dontChange + Tuple2(xExpended, yExpended), functionsList)
    if(fakeWallsExpand._2)
      fakeWallsExpand
    else {
      if(checkIsInside(Queue[(Int, Int)]((xExpended, yExpended)), List[(Int, Int)](), fakeWallsExpand._1, false))
        (fakeWallsExpand._1, true)
      else
        (game, false)
    }
  }
}
