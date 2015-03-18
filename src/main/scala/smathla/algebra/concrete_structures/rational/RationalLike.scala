package smathla.algebra.concrete_structures.rational

import smathla.Types._
import smathla.algebra.concrete_structures.integer.{Integer64, IntegerLike, Integer}
import smathla.algebra.structures.{Field, FieldElem}


case class RationalLike[A <: IntegerLike[A]](val numerator: A, val denominator: A) extends FieldElem[RationalLike[A]] {

  override def *(a: RationalLike[A]) = new RationalLike(n * a.n, d * a.d)
  override def +(a: RationalLike[A]) = new RationalLike(n * a.d + a.n * d, d * a.d)
  override def unary_- = new RationalLike[A](-n, d)
  override def unary_~ = new RationalLike[A](d, n)
  override def toString() = n.toString + "/" + d.toString
  override def isZero = n.isZero && !d.isZero
  override def isUnit = n == d && !n.isZero

  private def n = numerator
  private def d = denominator

  def reduce = new RationalLike[A](n / n.gcd(d), d / n.gcd(d))
}

object Rational extends Field[Rational] {

  def zero = new Rational(Integer.zero, Integer.unit)
  def unit = new Rational(Integer.unit, Integer.unit)
  def apply(numerator: Integer, denominator: Integer) = new Rational(numerator, denominator)
  def apply(numerator: Integer) = new Rational(numerator, Integer.unit)
  def unapply(that: Rational) = Some(that.numerator, that.denominator)
}

object Rational64 extends Field[Rational64] {

  def zero = new Rational64(Integer64.zero, Integer64.unit)
  def unit = new Rational64(Integer64.unit, Integer64.unit)
  def apply(numerator: Integer64, denominator: Integer64) = new Rational64(numerator, denominator)
  def apply(numerator: Integer64) = new Rational64(numerator, Integer64.unit)
  def unapply(that: Rational64) = Some(that.numerator, that.denominator)

}

