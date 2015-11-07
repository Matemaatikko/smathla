package smathla.core.geometry

import shapeless.Nat
import smathla.core.algebra.structures.impl.real.RealLike
import smathla.core.algebra.structures.vectorspace.CoordinateSpaceElem

class RealVector[N <: Nat, R <: RealLike[R]](private[geometry] val vector: smathla.core.Vector[N, R]) extends CoordinateSpaceElem[N, R, RealVector[N, R]]{

  def get(i: Int) = vector.get(i)
  def apply(i: Int): Option[R] = vector.apply(i)

  def +(vector: RealVector[N, R]) = new RealVector[N, R](this.vector + vector.vector)
  def isZero: Boolean = vector.forall(_.isZero)
  def unary_-  = new RealVector[N, R](vector.map(-_))
  def *(scalar: R) = new RealVector[N, R](vector*scalar)

  /**
    * Scalar product of vectors.
    */
  def *(vector: RealVector[N, R]): R = this.vector * vector.vector

  def length: R = smathla.calculus.sqrt(vector * vector)

  def /(r: R) = vector.map(_ / r)

  def unit = this / length
}

object RealVector{
  def apply[N <: Nat, R <: RealLike[R]](point1: Point[N, R], point2: Point[N, R]) = realVector(point1.vector - point2.vector)
  implicit def realVector[N <: Nat, R <: RealLike[R]](vector: smathla.core.Vector[N, R]) = new RealVector[N, R](vector)
}