
package Math.Geometry

import smathla.calculus
import smathla.core.algebra.structures.impl.complex.{Complex, ComplexLike}
import smathla.core.algebra.structures.impl.real.RealLike

import scala.reflect.ClassTag

//TODO all untested
case class Point2D[A <: RealLike[A]](x: A, y: A) {

  /*def this() {
    this(RealLike.zero[A], RealLike.zero[A])
  }*/

  def distance(point: Point2D[A]) = calculus.sqrt((point.x - this.x) * (point.x - this.x) + (point.y - this.y) * (point.y - this.y))

  def +(v: Vector2D[A]) = new Point2D(x + v.x, y + v.y)

  def -(v: Vector2D[A]) = new Point2D(x - v.x, y - v.y)

  def translate(v: Vector2D[A]) = this + v

  //def rotate(origin: Point2D[A], angle: A) = (ComplexLike.polar(angle, RealLike.unit[A])*(this-origin.toVector()).toComplex).toPoint + origin.toVector()

  def toComplex = ComplexLike(x, y)

  def toVector() = new Vector2D(x, y)
}

object Point2D{

  //def origin[A <: RealLike[A]: ClassTag] = new Point2D(RealLike.zero[A], RealLike.zero[A])

}
