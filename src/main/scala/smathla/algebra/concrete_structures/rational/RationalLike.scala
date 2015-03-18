package smathla.algebra.concrete_structures.rational

import smathla.Types.Rational
import smathla.algebra.concrete_structures.integer.{IntegerInf, Integer64, IntegerLike, Integer}
import smathla.algebra.structures.FieldElem


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


  // TODO re-document
  /**
   * Returns new rational number equal with this rational number
   * with greatest common divisor of numerator and denominator is 1.
   *
   */
  def reduce = new RationalLike[A](n / n.gcd(d), d / n.gcd(d))
}

