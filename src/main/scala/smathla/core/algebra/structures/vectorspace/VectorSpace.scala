package smathla.core.algebra.structures.vectorspace

import smathla.core.algebra.definitions.Dimension
import smathla.core.algebra.structures.field.FieldElem


/**
  * A vector space V over field F is defined by following axioms:
  *  1. u + ( v + w) == (u + v) + w (associativity)
  *  2. u + v == v + u (commutativity)
  *  3. There exists 0 in V such that 0 + v == v for all v in V (identity element)
  *  4. For every v in V there exists element -v in V such that v + (-v) = 0 (inverse element)
  *  5. Fol all a, b in F and v in V holds: a*(b*v) == (a*b)*v
  *  6. There exists 1 in F such that 1*v == v for all v in V
  *  7. For all a in F and u, v in V holds: a*(u + v) = a*u + a*v
  *  8. For all a, b in F and v in V holds: (a + b)*v = a*v + b*v
  */
trait VectorSpaceElem[F <: FieldElem[F], V <: VectorSpaceElem[F, V]] extends RightModuleElem[F, V] {

  def +(vector: V): V
  def isZero: Boolean
  def unary_- : V
  def *(scalar: F): V
}

trait VectorSpace[F <: FieldElem[F], V <: VectorSpaceElem[F, V]] extends RightModule[F, V]{
  def unit: V
  def zero: V
  def dimension: Dimension
}
