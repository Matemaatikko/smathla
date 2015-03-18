package smathla.algebra.concrete_structures.Real64

import smathla.algebra.concrete_structures.real.RealLike
import smathla.algebra.definitions.{Equal, Higher, Lower}
import smathla.algebra.structures.{Euclidean}

case class Real64(private val i: Double) extends RealLike[Real64]{

  override def *(a: Real64) = new Real64(a.i * this.i)
  override def +(a: Real64) = new Real64(a.i + this.i)
  override def unary_- = new Real64(-i)

  override def unary_~ = new Real64(1.0 / i)

  override def isPositive = i >= 0.0
  override def toString() = i.toString
  override def isZero = i == 0.0
  override def isUnit = i == 1.0

  override def compareTo(that: Real64) = if (i < that.i) Lower else if (i == that.i) Equal else Higher

  def toDouble = i
}

object Real64{
  def zero = Real64(0.0)
  def unit = Real64(1.0)
}