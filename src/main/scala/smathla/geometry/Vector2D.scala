package Math.Geometry

class Vector2D(x: Double, y: Double) {
  def this(p0: Point2D, p1: Point2D) {
    this(p1.getX - p0.getX, p1.getY - p0.getY)
  }

  def +(v: Vector2D) = new Vector2D(x + v.getX, y + v.getY)

  def getX = x

  def getY = y

  def unary_- = new Vector2D(-x, -y)

  def normal = new Vector2D(y, -x)

  def *(value: Double) = new Vector2D(value * x, value * y)

  def unit() = this / length

  def /(value: Double) = new Vector2D(x / value, y / value)

  def length = math.sqrt(x * x + y * y)
}
