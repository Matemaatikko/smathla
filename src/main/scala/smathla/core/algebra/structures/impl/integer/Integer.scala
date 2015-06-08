package smathla.core.algebra.structures.impl.integer

import smathla.core.algebra.definitions.{Equal, Higher, Lower}
import smathla.core.algebra.structures.impl.real.Real
import smathla.core.algebra.structures.{Euclidean}

case class Integer(private val i: Int) extends IntegerLike[Integer] {

  override def *(a: Integer) = new Integer(a.i * this.i)
  override def +(a: Integer) = new Integer(a.i + this.i)

  override def unary_- = new Integer(-i)

  override def toInt = i
  override def isPositive = i > 0

  def %(a: Integer) = new Integer(this.i % a.i)
  def /(a: Integer) = new Integer(this.i / a.i)

  override def toString() = i.toString
  override def isZero = i == 0
  override def isUnit = i == 1
  override def compareTo(that: Integer) = if (i < that.i) Lower else if (i == that.i) Equal else Higher
}

object Integer extends Euclidean[Integer] {

  def zero = new Integer(0)
  def unit = new Integer(1)

  implicit def Long2Integer(l: Long) = new Integer(l.toInt)
  implicit def Int2Integer(i: Int) = new Integer(i)
}