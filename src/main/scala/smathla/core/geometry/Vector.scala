package smathla.core.geometry

import shapeless.Nat
import smathla.core.algebra.structures.impl.real.RealLike

/**
 * Help class for
 */
class RealVector[N <: Nat, R <: RealLike[R]](private val vector: smathla.core.Vector[N, R]) {

  def length: R = smathla.calculus.sqrt(vector * vector)

  def /(r: R) = vector.map(_ / r)

  def unit = this / length
}

object RealVector{
  implicit def realVector[N <: Nat, R <: RealLike[R]](vector: smathla.core.Vector[N, R]) = new RealVector[N, R](vector)
}