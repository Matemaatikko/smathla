package smathla.core.algebra.definitions


trait Dimension

case class Finite(dimension: Int) extends Dimension
case object Infinite extends Dimension