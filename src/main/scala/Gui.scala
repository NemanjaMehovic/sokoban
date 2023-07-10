import Solver.History

import java.awt.Color
import javax.swing.SwingUtilities
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event.KeyTyped


class BoardCanvas(mainWindow: MainWindow, var game: Game) extends Component{

  minimumSize = new Dimension(mainWindow.gameWidth, mainWindow.gameHeight)

  @tailrec
  private def draw(x: Int, y: Int, map: List[List[Zone]], g: Graphics2D, w: Int, h: Int): Unit = {
    val maxY = map.length
    val maxX = map.head.length
    if(y != maxY){
        if(x == maxX)
          draw(0, y + 1, map, g, w, h)
        else{
          g.setColor(Color.BLACK)
          g.fillRect(x * w, y * h, w, h)
          map(y)(x) match {
            case _: Tile => g.setColor(Color.WHITE)
            case _: Wall => g.setColor(Color.DARK_GRAY)
            case _: Hero => g.setColor(Color.GREEN)
            case _: Box => g.setColor(Color.ORANGE)
            case _: Finish => g.setColor(Color.RED)
            case _: BoxOnFinish => g.setColor(Color.BLUE)
            case _: HeroOnFinish => g.setColor(Color.GREEN)
          }
          g.fillRect(x * w + 2, y * h + 2, w - 2, h - 2)
          draw(x + 1, y, map, g, w, h)
        }
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    val width  = mainWindow.gameWidth
    val height = mainWindow.gameHeight
    val mapWidth = game.map.head.length
    val mapHeight = game.map.length
    val boxWidth = width/mapWidth
    val boxHeight = height/mapHeight
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(Color.BLACK)

    g.fillRect(0, 0, width, height)

    draw(0, 0, game.map, g, boxWidth, boxHeight)
  }
}

class Animator(var parent: GamePanel, moves: List[Char]) extends Thread{

  private var running = false

  override def run(): Unit = {
    running = true
    for(move <- moves){
      if(running)
        Thread.sleep(1000)
      callF(_=>{
        if (running)
          SwingUtilities.invokeLater(() => parent.updateBoard(move))
      })
    }
    running = false
    println("Thread ending")
  }

  private def callF(f:Unit => Unit ) = {
    synchronized{
      f()
    }
  }

  def forceStop(): Unit = {
    callF(_ => running = false)
  }

  def isRunning(): Boolean = {
    running
  }
}

class GamePanel(mainWindow: MainWindow, game: Game, var history: History[Game], val animator: Animator = null, functionsList: List[(EditFunctions, String)]) extends BoxPanel(Orientation.Horizontal){

  private val canvas = new BoardCanvas(mainWindow, game)
  private val thisPanel = this
  contents += canvas
  contents += new BoxPanel(Orientation.Vertical){
    contents += Swing.HStrut(0)
    contents += new BorderPanel {
      val tmp = if(animator == null){
        new Button(Action("Play file") {
          val fileChooser = new FileChooser()
          fileChooser.title = "Pick a map"
          val vr = fileChooser.showDialog(mainWindow, "Pick")
          if (vr == FileChooser.Result.Approve) {
            val tryMovesList = Game.getMovesFromFile(fileChooser.peer.getSelectedFile.toString)
            if(tryMovesList.isFailure){
              Dialog.showMessage(mainWindow, tryMovesList.failed.get.getMessage, title="Error")
              giveFocus()
            }
            else{
              val newAnimator = new Animator(thisPanel, tryMovesList.get)
              mainWindow.setNewContents(new GamePanel(mainWindow, game, new History[Game](List[Game]()), newAnimator, functionsList))
            }
          }
          else
            giveFocus()
        }) -> Position.Center
      }
      else {
        new Button(Action("Stop playing") {
          animator.forceStop()
          giveFocus()
        }) -> Position.Center
      }
      layout += tmp
      minimumSize  = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Solve") {
        if (animator == null || !animator.isRunning()) {
          val solverOption = Solver.startSolving(game)
          if(solverOption.isDefined){
            val solutionList = solverOption.get
            Game.saveMovesToFile("path.txt", solutionList)
            Dialog.showMessage(mainWindow, "Solution saved to path.txt", title="Saved")
          }
          else
            Dialog.showMessage(mainWindow, "No solution found", title="Error")
        }
        giveFocus()
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Undo") {
        if (animator == null || !animator.isRunning()) {
          val optionGame = history.getGame()
          history = history.undo()
          if(optionGame.isDefined){
            canvas.game = optionGame.get
            canvas.repaint()
          }
        }
        giveFocus()
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Back") {
        if (animator == null || !animator.isRunning()) {
          mainWindow.setNewContents(new MainPanel(mainWindow, "", functionsList))
        }
        giveFocus()
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.HStrut(0)
    maximumSize = new Dimension(140, 500)
    background = Color.BLACK
  }
  background = Color.BLACK

  def updateBoard(key: Char): Unit = {
    val moveTuple = canvas.game.move(key)
    if(moveTuple._2){
      val newGame = moveTuple._1
      history = history.add(canvas.game)
      canvas.game = newGame
      canvas.repaint()
      requestFocus()
      checkIfDone(canvas.game)
    }
  }

  private def giveFocus() = {
    requestFocus()
  }

  private def checkIfDone(game: Game) = {
    if (game.isDone()) {
      if (animator != null && animator.isRunning())
        animator.forceStop()
      Dialog.showMessage(mainWindow, "You win!", title = "Winner")
      mainWindow.setNewContents(new MainPanel(mainWindow, "", functionsList))
    }
  }

  listenTo(keys)
  reactions += {
    case KeyTyped(_, key, _, _) =>
      key.toLower match {
        case 'w' => if (animator == null || !animator.isRunning()) updateBoard('U')
        case 'a' => if (animator == null || !animator.isRunning()) updateBoard('L')
        case 's' => if (animator == null || !animator.isRunning()) updateBoard('D')
        case 'd' => if (animator == null || !animator.isRunning()) updateBoard('R')
        case _ => //do nothing
      }
  }

  focusable = true
  requestFocus()
  if(animator != null) {
    animator.parent = thisPanel
    animator.start()
  }
}

class EditFunctions(f:Game => Option[Game]){
  def execute(game: Game): Option[Game] = {
    f(game)
  }
}

object EditFunctions{

  def AddRow(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val res = Dialog.showConfirmation(mainWindow, "Add at beginning", optionType = Dialog.Options.YesNo)
    val newGame = GameEditor.addRow(res == Dialog.Result.Yes, game)
    Some(newGame)
  }

  def AddCol(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val res = Dialog.showConfirmation(mainWindow, "Add at beginning", optionType = Dialog.Options.YesNo)
    val newGame = GameEditor.addColumn(res == Dialog.Result.Yes, game)
    Some(newGame)
  }

  def RemoveRow(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val res = Dialog.showConfirmation(mainWindow, "Remove at beginning", optionType = Dialog.Options.YesNo)
    val newGame = GameEditor.removeRow(res == Dialog.Result.Yes, game)
    Some(newGame)
  }

  def RemoveCol(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val res = Dialog.showConfirmation(mainWindow, "Remove at beginning", optionType = Dialog.Options.YesNo)
    val newGame = GameEditor.removeColumn(res == Dialog.Result.Yes, game)
    Some(newGame)
  }

  def Inversion(game: Game): Option[Game] = {
    val newGame = GameEditor.inverse(game)
    Some(newGame)
  }

  def Minimise(game: Game): Option[Game] = {
    val newGame = GameEditor.minimise(Queue[(Int, Int)]((game.heroX, game.heroY)), List[(Int, Int)](), game, Set[(Int, Int)]())
    Some(newGame)
  }

  def Swap(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val r = Dialog.showInput(mainWindow, "Enter x y and zone type(Tile, Wall, Hero, Box, Finish)", initial = "0 0 Wall")
    r match {
      case Some(s) =>
        val array = s.trim.split(" ")
        if (array.length == 3) {
          try {
            val x = array(0).toInt
            val y = array(1).toInt
            if (x >= game.map.head.length || x < 0 || y >= game.map.length || y < 0) {
              return None
            }
            array(2) match {
              case "Wall" => Option(GameEditor.swapZone(x, y, new Wall, game))
              case "Tile" => Option(GameEditor.swapZone(x, y, new Tile, game))
              case "Hero" => Option(GameEditor.swapZone(x, y, new Hero, game))
              case "Box" => Option(GameEditor.swapZone(x, y, new Box, game))
              case "Finish" => Option(GameEditor.swapZone(x, y, new Finish, game))
            }
          } catch {
            case _: Exception => None
          }
        }
        else
          None
      case None => None
    }
  }

  def Filter(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val r = Dialog.showInput(mainWindow, "Enter x y and N", initial = "0 0 0")
    r match {
      case Some(s) =>
        val array = s.trim.split(" ")
        if (array.length == 3) {
          try {
            val x = array(0).toInt
            val y = array(1).toInt
            val N = array(2).toInt
            if (x >= game.map.head.length || x < 0 || y >= game.map.length || y < 0 || N < 0) {
              return None
            }
            Option(GameEditor.filter(x, y, game, N))
          } catch {
            case _: Exception => None
          }
        }
        else
          None
      case None => None
    }
  }

  def Fractal(mainWindow: MainWindow)(game: Game): Option[Game] = {
    val r = Dialog.showInput(mainWindow, "Enter x and y", initial = "0 0")
    r match {
      case Some(s) =>
        val array = s.trim.split(" ")
        if (array.length == 2) {
          try {
            val x = array(0).toInt
            val y = array(1).toInt
            if (x >= game.map.head.length || x < 0 || y >= game.map.length || y < 0) {
              return None
            }
            val res = GameEditor.fractal(x, y, game, Set[(Int, Int)](), Set[(Int, Int)]())
            Option(res._1)
          } catch {
            case _: Exception => None
          }
        }
        else
          None
      case None => None
    }
  }

  def NamedSequence(functionsList: List[EditFunctions])(game: Game): Option[Game] = {

    @tailrec
    def executeList(game: Game, fList: List[EditFunctions]): Option[Game] = {
      if (fList.isEmpty)
        return Option(game)
      val f = fList.head
      val gameOption = f.execute(game)
      if (gameOption.isDefined)
        executeList(gameOption.get, fList.drop(1))
      else
        Option(game)
    }

    executeList(game, functionsList)
  }

  def Composition(functionsList: List[EditFunctions])(game: Game): Option[Game] = {

    @tailrec
    def executeList(game: Game, fList: List[EditFunctions]): Option[Game] = {
      if (fList.isEmpty)
        return Option(game)
      val f = fList.head
      val gameOption = f.execute(game)
      if (gameOption.isDefined)
        executeList(gameOption.get, fList.drop(1))
      else
        None
    }

    executeList(game, functionsList)
  }
}

class EditPanel(mainWindow: MainWindow, game: Game, var history: History[Game], isRecording: Boolean, functionsList: List[(EditFunctions, String)]) extends BoxPanel(Orientation.Horizontal){

  private var currComposition: List[EditFunctions] = List[EditFunctions]()

  private def executeFunction(fun: EditFunctions, exGame: Game) = {
    val res = fun.execute(exGame)
    if(res.isDefined) {
      history = history.add(canvas.game)
      canvas.game = res.get
      canvas.repaint()
    }
    else
      Dialog.showMessage(mainWindow, "Bad format")
  }

  private val canvas = new BoardCanvas(mainWindow, game)
  contents += canvas
  contents += new BoxPanel(Orientation.Vertical) {
    contents += Swing.HStrut(0)
    contents += new BorderPanel {
      layout += new Button(Action("Add Row") {
        val f:Game => Option[Game] = EditFunctions.AddRow(mainWindow)
        if(!isRecording){
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))

      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Add Col") {
        val f:Game => Option[Game] = EditFunctions.AddCol(mainWindow)
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))

      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Remove Row") {
        val f:Game => Option[Game] = EditFunctions.RemoveRow(mainWindow)
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Remove Col") {
        val f:Game => Option[Game] = EditFunctions.RemoveCol(mainWindow)
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Swap zone") {
        val f:Game => Option[Game] = EditFunctions.Swap(mainWindow)
        if(!isRecording){
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Inversion") {
        val f:Game => Option[Game] = EditFunctions.Inversion
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Minimise") {
        val f:Game => Option[Game] = EditFunctions.Minimise
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Filter") {
        val f:Game => Option[Game] = EditFunctions.Filter(mainWindow)
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Fractal") {
        val f:Game => Option[Game] = EditFunctions.Fractal(mainWindow)
        if (!isRecording) {
          executeFunction(new EditFunctions(f), canvas.game)
        }
        else
          currComposition = currComposition.appended(new EditFunctions(f))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      val tmp = if(!isRecording) {
        new Button(Action("Composition") {
          mainWindow.setNewContents(new EditPanel(mainWindow, game, new History[Game](List[Game]()), true, functionsList))
        }) -> Position.Center
      }
      else{
        new Button(Action("Stop recording") {
          val optionResult = Dialog.showConfirmation(mainWindow, "Is it a named sequence?", optionType = Dialog.Options.YesNo)
          if(optionResult == Dialog.Result.Yes){
            val nameResult = Dialog.showInput(mainWindow, "Enter sequence name", initial = "Name " + functionsList.length)
            if(nameResult.isDefined) {
              val f:Game => Option[Game] = EditFunctions.NamedSequence(currComposition)
              mainWindow.setNewContents(new EditPanel(mainWindow, game, new History[Game](List[Game]()), false, functionsList.appended((new EditFunctions(f), nameResult.get))))
            } else{
              Dialog.showMessage(mainWindow, "Name not given giving default name: Name " + functionsList.length)
              val f:Game => Option[Game] = EditFunctions.NamedSequence(currComposition)
              mainWindow.setNewContents(new EditPanel(mainWindow, game, new History[Game](List[Game]()), false, functionsList.appended((new EditFunctions(f), "Name " + functionsList.length))))
            }
          }
          else {
            val f: Game => Option[Game] = EditFunctions.Composition(currComposition)
            mainWindow.setNewContents(new EditPanel(mainWindow, game, new History[Game](List[Game]()), false, functionsList.appended((new EditFunctions(f), "Composition " + functionsList.length))))
          }
        }) -> Position.Center
      }
      layout += tmp
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Play") {
        val dialog = new Dialog(mainWindow)
        val scrollPane = new ScrollPane()
        val tmpPanel = new BoxPanel(Orientation.Vertical)
        if(functionsList.isEmpty)
          tmpPanel.contents += new Label("No compositions exists")
        else
          functionsList.foreach(f => {
            tmpPanel.contents += new Button(Action(f._2) {
              if (!isRecording){
                dialog.closeOperation()
                executeFunction(f._1, canvas.game)
              }
              else
                currComposition = currComposition.appended(f._1)
            })
          })
        scrollPane.contents = tmpPanel
        scrollPane.maximumSize = new Dimension(300, 400)
        dialog.contents = scrollPane
        dialog.minimumSize = new Dimension(150, 200)
        dialog.setLocationRelativeTo(mainWindow)
        dialog.open()
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Save") {
        val res = Game.saveGame(canvas.game)
        if(!res.isFailure)
          Dialog.showMessage(mainWindow, "Map successfully saved")
        else
          Dialog.showMessage(mainWindow, res.failed.get.getMessage)
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Undo") {
        val optionGame = history.getGame()
        history = history.undo()
        if (optionGame.isDefined) {
          canvas.game = optionGame.get
          canvas.repaint()
        }
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.VStrut(5)
    contents += new BorderPanel {
      layout += new Button(Action("Back") {
        mainWindow.setNewContents(new MainPanel(mainWindow, "", functionsList))
      }) -> Position.Center
      minimumSize = new Dimension(140, 25)
    }
    contents += Swing.HStrut(0)
    maximumSize = new Dimension(140, 500)
    background = Color.BLACK
  }
  background = Color.BLACK
}

class MainPanel(mainWindow: MainWindow, file: String, functionsList: List[(EditFunctions, String)]) extends BoxPanel(Orientation.Vertical){

  contents += Swing.HStrut(0)
  contents += new BorderPanel{
    layout += new Button(Action("Play"){
      if(file.isEmpty)
        Dialog.showMessage(mainWindow, "No map selected", title="Need map")
      else{
        val tryGame = Game.createGame(file)
        if(tryGame.isFailure)
          Dialog.showMessage(mainWindow, tryGame.failed.get.getMessage, title="Error")
        else{
          val game = tryGame.get
          try{
            if(game.isValidMap())
              mainWindow.setNewContents(new GamePanel(mainWindow, game, new History[Game](List[Game]()), null, functionsList))
            else
              Dialog.showMessage(mainWindow, "Not a valid game map. P.S this should never appear", title="Error")
          }catch{
            case exception: Exception => Dialog.showMessage(mainWindow, exception.getMessage, title="Error")
          }
        }
      }
    }) -> Position.Center
    maximumSize = new Dimension(100, 25)
  }
  contents += Swing.VStrut(5)
  contents += new BorderPanel{
    layout += new Button(Action("Edit"){
      if (file.isEmpty)
        Dialog.showMessage(mainWindow, "No map selected", title = "Need map")
      else {
        val tryGame = Game.createGame(file)
        if (tryGame.isFailure)
          Dialog.showMessage(mainWindow, tryGame.failed.get.getMessage, title = "Error")
        else {
          val game = tryGame.get
          try {
            if (game.isValidMap())
              mainWindow.setNewContents(new EditPanel(mainWindow, game, new History[Game](List[Game]()), false, functionsList))
            else
              Dialog.showMessage(mainWindow, "Not a valid game map. P.S this should never appear", title = "Error")
          } catch {
            case exception: Exception => Dialog.showMessage(mainWindow, exception.getMessage, title = "Error")
          }
        }
      }
    }) -> Position.Center
    maximumSize = new Dimension(100, 25)
  }
  contents += Swing.VStrut(5)
  contents += new BorderPanel {
    val fileChooser = new FileChooser()
    fileChooser.title = "Pick a map"
    layout += new Button(Action("Pick a map"){
      val vr = fileChooser.showDialog(mainWindow, "Pick")
      if(vr == FileChooser.Result.Approve){
        mainWindow.setNewContents(new MainPanel(mainWindow, fileChooser.peer.getSelectedFile.toString, functionsList))
      }
      else
        println("No file picked")
    }) -> Position.Center
    maximumSize = new Dimension(125, 25)
  }
  contents += Swing.HStrut(0)
}



class MainWindow extends MainFrame{
  val gameWidth = 500
  val gameHeight = 500
  title = "Sokoban"
  setNewContents(new MainPanel(this, "", List[(EditFunctions, String)]()))
  centerOnScreen()

  def setNewContents(panel:Panel): Unit = {
    contents = panel
    size = new Dimension(640, 540)
    resizable = false
    panel.requestFocus()
  }

  import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE

  peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

  override def closeOperation() {
    contents.head match {
      case panel:GamePanel =>
        if(panel.animator != null && panel.animator.isRunning())
          panel.animator.forceStop()
        super.closeOperation()
      case _ =>
        super.closeOperation()
    }
  }
}
