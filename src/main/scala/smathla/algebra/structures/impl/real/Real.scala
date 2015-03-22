package smathla.algebra.structures.impl.real

import smathla.algebra.definitions.{Equal, Higher, Lower}
import smathla.algebra.structures.{Euclidean}

case class Real(private val value: Float) extends RealLike[Real] {

  override def *(a: Real) = new Real(a.value * this.value)
  override def +(a: Real) = new Real(a.value + this.value)
  override def unary_- = new Real(-value)

  override def unary_~ = new Real(1f / value)


  override def toString() = value.toString
  override def isZero = value == 0f
  override def isUnit = value == 1f
  override def isPositive = value > 0f

  private val precision = 1e-5

  override def equals(any: Any) = any match {
    case Real(0f) => value <= precision
    case Real(value0) if value != 0f => math.abs(value/value0 - 1) <= precision
    case Real(value0) if value == 0f => math.abs(value - value0) <= precision
    case _ => false
  }

  override def compareTo(that: Real) = if (value < that.value) Lower else if (value == that.value) Equal else Higher

  def toFloat = value
}

object Real{
  def zero = Real(0f)
  def unit = Real(1f)

  implicit def float2Real(f: Float) = Real(f)
}