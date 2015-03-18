package smathla.algebra.concrete_structures.integer

import smathla.algebra.definitions.TotallyOrderable
import smathla.algebra.structures.{EuclideanElem}

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

  def isPositive: Boolean
  def isNegative: Boolean = !isZero && !isPositive
}

object IntegerLike{

  val IntegerTag = implicitly[ClassTag[Integer]]
  val Integer64Tag = implicitly[ClassTag[Integer64]]

  def unit[A <: IntegerLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case IntegerTag => Integer.unit
    case Integer64Tag => Integer64.unit
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def zero[A <: IntegerLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case IntegerTag => Integer.zero
    case Integer64Tag => Integer64.zero
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]
}
