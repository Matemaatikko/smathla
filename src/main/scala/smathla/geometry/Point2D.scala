
package Math.Geometry

class Point2D(x: Double, y: Double) {

  def this() {
    this(0, 0)
  }

  def distance(point: Point2D) = math.sqrt((point.getX - this.x) * (point.getX - this.x) + (point.getY - this.y) * (point.getY - this.y))

  def getX = x

  def getY = y

  def +(v: Vector2D) = new Point2D(x + v.getX, y + v.getY)

  def -(v: Vector2D) = new Point2D(x - v.getX, y - v.getY)
}
