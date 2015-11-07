package smathla.core.algebra.definitions

import smathla.core.algebra.structures.impl.real.RealLike

/**
  * Following axioms must hold: (mark d = distance)
  *   1. d(x, y) >= 0
  *   2. d(x, y) == 0 iff x == y
  *   3. d(x, y) == d(y, x)
  *   4. d(x, z) <= d(x, y) + d(y, z)
  */
trait Metrics[M] {
  def distance[R <: RealLike[R]](x: M, y: M): R
}
