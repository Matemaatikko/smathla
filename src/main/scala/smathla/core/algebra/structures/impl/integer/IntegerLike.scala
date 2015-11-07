package smathla.core.algebra.structures.impl.integer

import smathla.core.algebra.definitions.TotallyOrderable
import smathla.core.algebra.structures.ring.EuclideanElem

import scala.reflect.ClassTag


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

  def abs = this*signum

  def signum: A

  def isPositive: Boolean
  def isNegative: Boolean = !isZero && !isPositive
}

object IntegerLike{

  val IntegerTag = implicitly[ClassTag[Integer]]
  val Integer64Tag = implicitly[ClassTag[Integer64]]
  val IntegerInfTag = implicitly[ClassTag[IntegerInf]]

  def unit[A <: IntegerLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case IntegerTag => Integer.unit
    case Integer64Tag => Integer64.unit
    case IntegerInfTag => IntegerInf.unit
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def zero[A <: IntegerLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case IntegerTag => Integer.zero
    case Integer64Tag => Integer64.zero
    case IntegerInfTag => IntegerInf.zero
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]
}
