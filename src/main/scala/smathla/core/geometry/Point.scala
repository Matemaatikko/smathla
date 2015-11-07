package smathla.core.geometry

import shapeless.{Sized, Nat}
import smathla.core._
import smathla.core.RealVector._
import smathla.core.algebra.definitions.{MetricSpaceElement, Metrics}
import smathla.core.algebra.structures.impl.real.RealLike

class Point[N <: Nat, R <: RealLike[R]](private[smathla] val vector: smathla.core.Vector[N, R]) extends MetricSpaceElement[Point[N, R], R]{

  def get(i: Int) = vector.get(i)
  def apply(i: Int) = vector(i)
  def distance(point: Point[N, R]): R = (vector - point.vector).length
  def +(vector: RealVector[N, R]) = new Point[N, R](this.vector + vector.vector)
  def -(vector: RealVector[N, R]) = new Point[N, R](this.vector - vector.vector)

  def translate(v: RealVector[N, R]) = this + v

  def toVector = realVector(vector)
}

object Point{
  def apply[N <: Nat, R <: RealLike[R]](array: Sized[Seq[R], N]) = new Point(new Vector(array))
}
