package smathla.algebra.structures.impl.real

import smathla.algebra.definitions.{Equal, Higher, Lower}
import smathla.algebra.structures.{Euclidean}

case class Real64(private val value: Double) extends RealLike[Real64]{

  override def *(a: Real64) = new Real64(a.value * this.value)
  override def +(a: Real64) = new Real64(a.value + this.value)
  override def unary_- = new Real64(-value)

  override def unary_~ = new Real64(1.0 / value)

  override def isPositive = value > 0.0
  override def toString() = value.toString
  override def isZero = value == 0.0
  override def isUnit = value == 1.0

  override def equals(any: Any) = any match {
    case Real64(0.0) => value == 0.0
    case Real64(value0) => math.abs(value/value0 - 1) <= 1e-15
    case _ => false
  }

  override def compareTo(that: Real64) = if (value < that.value) Lower else if (value == that.value) Equal else Higher

  def toDouble = value
}

object Real64{
  def zero = Real64(0.0)
  def unit = Real64(1.0)
}