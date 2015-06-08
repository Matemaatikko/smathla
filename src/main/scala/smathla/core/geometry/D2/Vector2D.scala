package Math.Geometry

import smathla.calculus
import smathla.core.algebra.structures.impl.real.RealLike

//TODO all untested
case class Vector2D[A <: RealLike[A]](val x: A, val y: A) {
  def this(p0: Point2D[A], p1: Point2D[A]) {
    this(p1.x - p0.x, p1.y - p0.y)
  }

  def +(v: Vector2D[A]) = new Vector2D(x + v.x, y + v.y)

  def unary_- = new Vector2D(-x, -y)

  def normal = new Vector2D(y, -x)

  def *(value: A) = new Vector2D(value * x, value * y)

  //scalar product/ dot product
  //TODO probably rename
  def dot(vector: Vector2D[A]) = x*vector.x + y*vector.y

  def unit() = this / length

  def /(value: A) = new Vector2D(x / value, y / value)

  def length = calculus.sqrt(x * x + y * y)
}
