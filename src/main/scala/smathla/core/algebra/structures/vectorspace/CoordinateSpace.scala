package smathla.core.algebra.structures.vectorspace

import shapeless.ops.nat.ToInt
import shapeless.{Sized, Nat}
import smathla.core.algebra.definitions.Finite
import smathla.core.algebra.structures.field.{FieldElem, DivisionRing}


/**
  * A vector space composed of all the n-tuples of a field F is known as a coordinate space.
  */
trait CoordinateSpaceElem[N <: Nat, F <: FieldElem[F], S <: CoordinateSpaceElem[N, F, S]] extends VectorSpaceElem[F, S]{

  def apply(i: Int): Option[F]

  def +(vector: S): S
  def -(vector: S): S = this + -vector
  def isZero: Boolean
  def unary_- : S
  def *(scalar: F): S

  /**
    * Scalar product of vectors.
    */
  def *(vector:S): F
}
/**
  * A vector space composed of all the n-tuples of a field F is known as a coordinate space.
  */
class CoordinateSpaceElemImpl[N <: Nat, F <: FieldElem[F]](private val vector: smathla.core.Vector[N, F]) extends VectorSpaceElem[F, CoordinateSpaceElemImpl[N, F]]{

  def apply(i: Int): Option[F] = vector.apply(i)

  def +(vector: CoordinateSpaceElemImpl[N, F]): CoordinateSpaceElemImpl[N, F] = new CoordinateSpaceElemImpl[N, F](this.vector + vector.vector)
  def isZero: Boolean = vector.forall(_.isZero)
  def unary_-  = new CoordinateSpaceElemImpl[N, F](vector.map(-_))
  def *(scalar: F): CoordinateSpaceElemImpl[N, F] = new CoordinateSpaceElemImpl[N, F](vector*scalar)

  /**
    * Scalar product of vectors.
    */
  def *(vector: CoordinateSpaceElemImpl[N, F]): F = this.vector * vector.vector
}

object CoordinateSpaceElemImpl {
  def apply[N <: Nat, F <: FieldElem[F]](array: Sized[Seq[F], N]) = new CoordinateSpaceElemImpl[N, F](new smathla.core.Vector[N, F](array))
}

trait CoordinateSpace[N <: Nat, F <: FieldElem[F], S <: CoordinateSpaceElem[N, F, S]] extends VectorSpace[F, S] {

  def dimension(implicit toInt: ToInt[N]) = Finite(Nat.toInt[N])
}
