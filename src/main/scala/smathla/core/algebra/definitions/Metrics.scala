package smathla.core.algebra.definitions

import smathla.core.algebra.structures.impl.real.RealLike

/**
  * Following axioms must hold: (mark d = distance)
  *   1. d(x, y) >= 0
  *   2. d(x, y) == 0 iff x == y
  *   3. d(x, y) == d(y, x)
  *   4. d(x, z) <= d(x, y) + d(y, z)
  */
trait Metrics[M, R <: RealLike[R]] {
  def distance(x: M, y: M): R
}

trait MetricSpaceElement[M, R <: RealLike[R]] {
  def distance(x: M): R
}

