package Math.Geometry.Container

import Math.Geometry.Point2D

class Union(containers: Container*) extends Container {

  def contains(point: Point2D) = !containers.forall(container => !container.contains(point))
}
