package smathla.algebra.structures.impl.rational

import smathla.Types._
import smathla.algebra.definitions.TotallyOrderable
import smathla.algebra.structures.impl.integer
import smathla.algebra.structures.impl.integer.{IntegerLike, Integer64}
import smathla.algebra.structures.{Field, FieldElem}

import scala.reflect.ClassTag


case class RationalLike[A <: IntegerLike[A]: ClassTag](val numerator: A, val denominator: A) extends FieldElem[RationalLike[A]] with TotallyOrderable[RationalLike[A]] {

  override def *(a: RationalLike[A]) = new RationalLike(numerator * a.numerator, denominator * a.denominator)
  override def +(a: RationalLike[A]) = new RationalLike(numerator * a.denominator + a.numerator * denominator, denominator * a.denominator)
  override def unary_- = new RationalLike[A](-numerator, denominator)
  override def unary_~ = new RationalLike[A](denominator, numerator)
  override def toString() = numerator.toString + "/" + denominator.toString
  override def isZero = numerator.isZero && !denominator.isZero
  override def isUnit = numerator == denominator && !numerator.isZero

  def abs = RationalLike[A](signum*numerator, denominator)
  def signum = if(isNegative) -IntegerLike.unit[A] else IntegerLike.unit[A]

  def isPositive: Boolean = (numerator.signum*denominator.signum).isPositive
  def isNegative: Boolean = !isZero && !isPositive

  def isNaN = denominator.isZero

  def compareTo(another: RationalLike[A]) = {
    val n1 = numerator.abs
    val d1 = denominator.abs
    val s1 = signum

    val n2 = another.numerator.abs
    val d2 = another.denominator.abs
    val s2 = another.signum
    (s1*n1*d2).compareTo(s2*n2*d1)
  }

  /**
   * This method returns nearest integer.
   * If there is two then returns greater.
   */
  def round: A = {
    val (q, r) = numerator.div(denominator)
    q + (if(r.abs +r.abs >= denominator.abs) r.signum*IntegerLike.unit[A] else IntegerLike.zero[A])
  }

  def reduce = new RationalLike[A](numerator / numerator.gcd(denominator), denominator / numerator.gcd(denominator))
}

object RationalLike{

  val IntegerTag = implicitly[ClassTag[Integer]]
  val Integer64Tag = implicitly[ClassTag[Integer64]]
  
  def unit[A <: IntegerLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case IntegerTag => Rational.unit
    case Integer64Tag => Rational64.unit
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def zero[A <: IntegerLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case IntegerTag => Rational.zero
    case Integer64Tag => Rational64.zero
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]
}

object Rational extends Field[Rational] {

  def zero = new Rational(integer.Integer.zero, integer.Integer.unit)
  def unit = new Rational(integer.Integer.unit, integer.Integer.unit)
  def apply(numerator: integer.Integer, denominator: integer.Integer) = new Rational(numerator, denominator)
  def apply(numerator: integer.Integer) = new Rational(numerator, integer.Integer.unit)
  def unapply(that: Rational) = Some(that.numerator, that.denominator)
}

object Rational64 extends Field[Rational64] {

  def zero = new Rational64(Integer64.zero, Integer64.unit)
  def unit = new Rational64(Integer64.unit, Integer64.unit)
  def apply(numerator: Integer64, denominator: Integer64) = new Rational64(numerator, denominator)
  def apply(numerator: Integer64) = new Rational64(numerator, Integer64.unit)
  def unapply(that: Rational64) = Some(that.numerator, that.denominator)

}

