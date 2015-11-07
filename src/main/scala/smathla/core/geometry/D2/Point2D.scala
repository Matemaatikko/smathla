
package Math.Geometry

import shapeless.Sized
import smathla.calculus
import smathla.core._
import smathla.core.algebra.structures.impl.complex.{Complex, ComplexLike}
import smathla.core.algebra.structures.impl.real.RealLike
import smathla.core.geometry.Point

import scala.reflect.ClassTag

//TODO all untested
class Point2D[R <: RealLike[R]: ClassTag](val point: Point[`2`, R]) {

  def x = point.get(0)
  def y = point.get(1)
  def rotate(origin: Point2D[R], angle: R) = (ComplexLike.polar(angle, RealLike.unit[R])*(this.toComplex - origin.toComplex)).toPoint + origin.point.toVector
  def toComplex = ComplexLike[R](point.toVector.get(0), point.toVector.get(1))
}

object Point2D{

  def origin[A <: RealLike[A]: ClassTag] = Point(Sized(RealLike.zero[A], RealLike.zero[A]))

}
