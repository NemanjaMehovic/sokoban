import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

object Solver {

  class History[T](val history: List[T]) {

    def add(value: T): History[T] = {
      new History[T](history.appended(value))
    }

    def undo(): History[T] = {
      if (history.length > 1)
        new History[T](history.dropRight(1))
      else
        new History[T](history)
    }

    def getGame(): Option[T] = {
      history.lastOption
    }

  }

  class GameState(val game:Game){

    private val data: Set[(Int, Int)] =
      (for{
        i <- game.map.indices
        j <- game.map.head.indices
        if(game.map(i)(j).isHero || game.map(i)(j).isMovable)
          tupleVal = (j, i)
      } yield tupleVal).toSet

    override def equals(obj: Any): Boolean = {
      obj match {
        case e: GameState =>
          if(e.data.subsetOf(data)) {
            e.data.forall( tuple =>{
              e.game.map(tuple._2)(tuple._1).getClass == game.map(tuple._2)(tuple._1).getClass
            })
          }
          else
            false
        case _ => false
      }
    }

    def isDeadEnd(): Boolean = {
      data.forall( tuple => {
        if(!game.map(tuple._2)(tuple._1).isInstanceOf[Box])
          false
        else{
          (game.map(tuple._2 + 1)(tuple._1).isWall || game.map(tuple._2 + 1)(tuple._1).isWall) &&
          (game.map(tuple._2)(tuple._1 + 1).isWall || game.map(tuple._2)(tuple._1 - 1).isWall)
        }
      })
    }

    @tailrec
    private def calculatedShortest(cord1:List[(Int, Int)], cord2:List[(Int, Int)], acc: Double):Double = {
      if(cord1.isEmpty || cord2.isEmpty)
        return acc

      val i = cord2.indexOf(cord2.minBy(tuple => Math.sqrt( Math.pow(cord1.head._1 - tuple._1, 2) + Math.pow(cord1.head._1 - tuple._1, 2))))
      val newCord1 = cord1.drop(1)
      val newCord2 = cord2.take(i) ++ cord2.drop(i + 1)
      calculatedShortest(newCord1, newCord2, acc + Math.sqrt(Math.pow(cord1.head._1 - cord2(i)._1, 2) + Math.pow(cord1.head._1 - cord2(i)._1, 2)))
    }

    @tailrec
    private def separate(elements: IndexedSeq[(Int, Int)], retVal:(List[(Int, Int)], List[(Int, Int)])): (List[(Int, Int)], List[(Int, Int)]) = {
      if(elements.isEmpty)
        return retVal
      game.map(elements.head._2)(elements.head._1) match {
        case _:Box =>
          separate(elements.drop(1), (retVal._1.appended(elements.head), retVal._2))
        case _:Finish =>
          separate(elements.drop(1), (retVal._1, retVal._2.appended(elements.head)))
      }
    }

    def heuristic: Double = {
      val tuples = for{
        i <- game.map.indices
        j <- game.map.head.indices
        if(game.map(i)(j).isInstanceOf[Box] || game.map(i)(j).isInstanceOf[Finish])
          tuple = (j, i)
      } yield tuple
      val separated = separate(tuples, (List[(Int, Int)](),List[(Int, Int)]()))
      val boxTuples = separated._1
      val finishTuples = separated._2
      1000 - (calculatedShortest(finishTuples, boxTuples, 0) + calculatedShortest(List[(Int, Int)]((game.heroX, game.heroY)), finishTuples,0))
    }
  }

  @tailrec
  def solve(games:mutable.PriorityQueue[(Game,List[Char])], history:Set[GameState]): Option[List[Char]] = {
    if(games.isEmpty)
      return None
    val currGameTuple = games.dequeue
    if(currGameTuple._1.isDone())
      return Option(currGameTuple._2)
    val currGameState = new GameState(currGameTuple._1)
    if(history.exists( state => state equals currGameState) || currGameState.isDeadEnd())
      solve(games, history)
    else {
      games.enqueue((currGameTuple._1.move('L')._1, currGameTuple._2.appended('L')),
        (currGameTuple._1.move('R')._1, currGameTuple._2.appended('R')),
        (currGameTuple._1.move('D')._1, currGameTuple._2.appended('D')),
        (currGameTuple._1.move('U')._1, currGameTuple._2.appended('U')))
      solve(games, history + currGameState)
    }
  }

  def startSolving(startGame:Game): Option[List[Char]] = {
    solve(mutable.PriorityQueue[(Game, List[Char])]((startGame, List[Char]()))(Ordering.by(tuple=>(new GameState(tuple._1)).heuristic)), Set[GameState]())
  }

  @tailrec
  def solveBFS(games:Queue[(Game, List[Char])], history:Set[GameState]): Option[List[Char]] = {
    if(games.isEmpty)
      return None
    val currTuple = games.dequeue
    val currGameTuple = currTuple._1
    if (currGameTuple._1.isDone())
      return Option(currGameTuple._2)
    val currGameState = new GameState(currGameTuple._1)
    if (history.exists(state => state equals currGameState) || currGameState.isDeadEnd())
      solveBFS(currTuple._2, history)
    else{
      solveBFS(currTuple._2.appendedAll(Queue[(Game, List[Char])](
        (currGameTuple._1.move('L')._1, currGameTuple._2.appended('L')),
        (currGameTuple._1.move('R')._1, currGameTuple._2.appended('R')),
        (currGameTuple._1.move('D')._1, currGameTuple._2.appended('D')),
        (currGameTuple._1.move('U')._1, currGameTuple._2.appended('U'))
      )), history + currGameState)
    }
  }

  def startSolvingBFS(startGame:Game): Option[List[Char]] = {
    solveBFS(Queue[(Game, List[Char])]((startGame, List[Char]())), Set[GameState]())
  }
}
