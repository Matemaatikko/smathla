package smathla.algebra.structures.impl.real

import smathla.algebra.definitions.TotallyOrderable
import smathla.algebra.structures.FieldElem
import smathla.algebra.structures.impl.real.Real64

import scala.reflect.ClassTag

trait RealLike[A <: RealLike[A]] extends FieldElem[A] with TotallyOrderable[A]{
  this: A =>

  def isPositive: Boolean
  def isNegative: Boolean = !isZero && !isPositive
}

object RealLike{

  val RealTag = implicitly[ClassTag[Real]]
  val Real64Tag = implicitly[ClassTag[Real64]]

  def unit[A <: RealLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case RealTag => Real.unit
    case Real64Tag => Real64.unit
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def zero[A <: RealLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case RealTag => Real.zero
    case Real64Tag => Real64.zero
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]
}