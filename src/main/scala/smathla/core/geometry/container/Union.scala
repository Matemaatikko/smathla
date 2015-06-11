package Math.Geometry.Container

class Union[A](containers: Container[A]*) extends Container[A] {

  def contains(element: A) = !containers.forall(container => !container.contains(element))
}
