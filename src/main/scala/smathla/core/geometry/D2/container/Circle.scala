package smathla.core.geometry.D2.container

import Math.Geometry.Container.Container
import Math.Geometry.Point2D
import smathla.core.algebra.structures.impl.real.RealLike

class Circle[R <: RealLike[R]](center: Point2D[R], radius: R) extends Container[Point2D[R]] {
  def contains(point: Point2D[R]) = center.distance(point) <= radius
}
