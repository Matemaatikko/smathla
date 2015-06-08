package Math.Geometry.Container

import Math.Geometry.Point2D

class Circle(center: Point2D, radius: Double) extends Container {
  def contains(point: Point2D) = center.distance(point) <= radius
}
