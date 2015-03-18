package smathla.algebra.concrete_structures.real

import smathla.algebra.definitions.TotallyOrderable
import smathla.algebra.structures.FieldElem

trait RealLike[A <: RealLike[A]] extends FieldElem[A] with TotallyOrderable[A]{
  this: A =>

  def isPositive: Boolean
  def isNegative: Boolean = !isZero && !isPositive
}
