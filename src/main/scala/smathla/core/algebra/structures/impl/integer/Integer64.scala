package smathla.core.algebra.structures.impl.integer

import smathla.core.algebra.definitions.{Equal, Higher, Lower, TotallyOrderable}
import smathla.core.algebra.structures.ring.Euclidean

case class Integer64(private val i: Long) extends IntegerLike[Integer64] {

  override def *(a: Integer64) = new Integer64(a.i * this.i)
  override def +(a: Integer64) = new Integer64(a.i + this.i)
  override def unary_- = new Integer64(-i)

  override def toInt = i.toInt
  override def isPositive = i > 0l

  def %(a: Integer64) = new Integer64(this.i % a.i)
  def /(a: Integer64) = new Integer64(this.i / a.i)

  override def toString() = i.toString

  override def isZero = i == 0l
  override def isUnit = i == 1l

  override def compareTo(that: Integer64) = if (i < that.i) Lower else if (i == that.i) Equal else Higher

  def toInteger = new Integer(i.toInt)

  override def signum = if(isNegative) -Integer64.unit else Integer64.unit
}

object Integer64 extends Euclidean[Integer64] {

  def zero = new Integer64(0l)
  def unit = new Integer64(1l)
  def apply(i: Int) = new Integer64(i.toLong)

  implicit def Long2IInteger64(l: Long) = new Integer64(l)
  implicit def Int2Integer64(i: Int) = new Integer64(i)
}