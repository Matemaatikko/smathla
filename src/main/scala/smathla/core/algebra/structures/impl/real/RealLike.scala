package smathla.core.algebra.structures.impl.real

import smathla.core.algebra.definitions.TotallyOrderable
import smathla.core.algebra.structures.DivisionRingElem

import scala.reflect.ClassTag

abstract class RealLike[A <: RealLike[A]: ClassTag] extends DivisionRingElem[A] with TotallyOrderable[A]{
  this: A =>

  def abs = this*signum

  def signum = if(isNegative) -RealLike.unit[A] else RealLike.unit[A]

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
