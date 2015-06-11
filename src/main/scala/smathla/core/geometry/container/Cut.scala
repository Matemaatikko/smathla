
package Math.Geometry.Container

class Cut[A](containers: Container[A]*) extends Container[A] {
  def contains(element: A) = containers.forall(container => container.contains(element))
}
