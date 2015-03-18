package smathla.algebra.concrete_structures.integer

import smathla.algebra.definitions.{Equal, Higher, Lower}
import smathla.algebra.structures.{Euclidean}

case class IntegerInf(private val i: BigInt) extends IntegerLike[IntegerInf] {

  override def *(a: IntegerInf) = new IntegerInf(a.i * this.i)
  override def +(a: IntegerInf) = new IntegerInf(a.i + this.i)

  override def unary_- = new IntegerInf(-i)

  override def toInt = i.toInt
  override def isPositive = i > BigInt(0)

  def %(a: IntegerInf) = new IntegerInf(this.i % a.i)
  def /(a: IntegerInf) = new IntegerInf(this.i / a.i)


  override def toString() = i.toString

  override def isZero = i == BigInt(0)
  override def isUnit = i == BigInt(1)

  override def compareTo(that: IntegerInf) = if (i < that.i) Lower else if (i == that.i) Equal else Higher

  def toInteger64 = new Integer64(i.toLong)
  def toInteger = new Integer(i.toInt)
}

object IntegerInf extends Euclidean[IntegerInf] {

  def zero = new IntegerInf(BigInt(0))
  def unit = new IntegerInf(BigInt(1))
  def apply(i: Int) = new IntegerInf(BigInt(i))
  def apply(i: Long) = new IntegerInf(BigInt(i))

  implicit def Long2IIntegerInf(l: Long) = new IntegerInf(l)
  implicit def Int2IntegerInf(i: Int) = new IntegerInf(i)
}