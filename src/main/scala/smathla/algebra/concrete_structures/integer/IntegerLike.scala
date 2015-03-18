package smathla.algebra.concrete_structures.integer

import smathla.algebra.definitions.TotallyOrderable
import smathla.algebra.structures.{EuclideanElem, Euclidean}


trait IntegerLike[A <: IntegerLike[A]] extends EuclideanElem[A] with TotallyOrderable[A]{
  this: A =>

  def toInt: Int

  /**
   * Returns remainder.
   */
  def %(a: A): A

  /**
   * Returns integer part of ordinary division
   */
  def /(a: A): A

  override def div(a: A) = (this / a, this % a)

  def isPositive: Boolean
  def isNegative: Boolean = !isZero && !isPositive

}
