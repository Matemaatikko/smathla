package smathla.core.algebra.structures.vectorspace

import shapeless.ops.nat.ToInt
import shapeless.{Sized, Nat}
import smathla.core.algebra.definitions.Finite
import smathla.core.algebra.structures.field.{FieldElem, DivisionRing}

/**
  * A vector space composed of all the n-tuples of a field F is known as a coordinate space.
  */
class CoordinateSpaceElem[N <: Nat, F <: FieldElem[F]](private val vector: smathla.core.Vector[N, F]) extends VectorSpaceElem[F, CoordinateSpaceElem[N, F]]{

  def apply(i: Int): Option[F] = vector.apply(i)

  def +(vector: CoordinateSpaceElem[N, F]): CoordinateSpaceElem[N, F] = new CoordinateSpaceElem[N, F](this.vector + vector.vector)
  def isZero: Boolean = vector.forall(_.isZero)
  def unary_-  = new CoordinateSpaceElem[N, F](vector.map(-_))
  def *(scalar: F): CoordinateSpaceElem[N, F] = new CoordinateSpaceElem[N, F](vector*scalar)

  /**
    * Scalar product of vectors.
    */
  def *(vector: CoordinateSpaceElem[N, F]): F = this.vector * vector.vector
}

object CoordinateSpaceElem {
  def apply[N <: Nat, F <: FieldElem[F]](array: Sized[Seq[F], N]) = new CoordinateSpaceElem[N, F](new smathla.core.Vector[N, F](array))
}



trait CoordinateSpace[N <: Nat, F <: FieldElem[F]] extends VectorSpace[F, CoordinateSpaceElem[N, F]] {

  def dimension(implicit toInt: ToInt[N]) = Finite(Nat.toInt[N])
}
