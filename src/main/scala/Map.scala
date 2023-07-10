abstract class Zone(){
  def canMoveTo: Boolean = false
  def isMovable: Boolean = false
  def isFinish: Boolean = false
  def isHero: Boolean = false
  def isWall: Boolean = false
  def moveTo(mvObject:Zone): Zone = null
  def moveFrom: Zone = null
}

class Tile() extends Zone{
  override def canMoveTo: Boolean = true

  override def moveTo(mvObject: Zone): Zone = {
    if (mvObject.isInstanceOf[BoxOnFinish] || mvObject.isInstanceOf[Box])
      return new Box
    new Hero
  }

  override def toString: String = "-"
}

class Wall() extends Zone{
  override def isWall: Boolean = true
  override def toString: String = "#"
}

class Hero() extends Zone{
  override def isHero: Boolean = true

  override def moveFrom: Zone = {
    new Tile
  }

  override def toString: String = "S"
}

class Box extends Zone{
  override def isMovable: Boolean = true

  override def moveFrom: Zone = {
    new Tile
  }

  override def toString: String = "x"
}

class Finish extends Zone{
  override def isFinish: Boolean = true

  override def canMoveTo: Boolean = true

  override def moveTo(mvObject:Zone): Zone = {
    if(mvObject.isInstanceOf[BoxOnFinish] || mvObject.isInstanceOf[Box])
      return new BoxOnFinish
    new HeroOnFinish
  }

  override def toString: String = "."
}

class BoxOnFinish extends Zone{
  override def isMovable: Boolean = true

  override def moveFrom: Zone = {
    new Finish
  }

  override def toString: String = "O"
}

class HeroOnFinish extends Zone{
  override def isHero: Boolean = true

  override def moveFrom: Zone = {
    new Finish
  }

  override def toString: String = "S"
}