package smathla.core.algebra.structures.impl.complex

import Math.Geometry.Point2D
import smathla.core.Types
import Types.{Complex64, Complex}
import smathla.core.algebra.structures.impl.real.{Real64, RealLike, Real}
import smathla.core.algebra.structures.{Field, DivisionRingElem}
import smathla.calculus

import scala.reflect.ClassTag


case class ComplexLike[A <: RealLike[A]](private val re: A, private val im: A) extends DivisionRingElem[ComplexLike[A]] {

  val magnitude: A = calculus.sqrt(re * re + im * im)
  val angle: A = calculus.atan2(im, re)

  override def *(a: ComplexLike[A]) = new ComplexLike[A](re * a.Re - im * a.Im, im * a.Re + re * a.Im)
  override def +(a: ComplexLike[A]) = new ComplexLike[A](re + a.Re, im + a.Im)
  override def unary_- = new ComplexLike[A](-re, -im)
  override def unary_~ = new ComplexLike[A](re / (abs * abs), -im / (abs * abs))

  def abs = magnitude

  override def isUnit = re.isUnit && im.isZero
  override def isZero = re.isZero && im.isZero

  def norm = re * re + im * im

  def conjugate = new ComplexLike[A](re, -im)

  def Re = re
  def Im = im

  override def toString() =
    if (im.isUnit && re.isZero) "i"
    else if (im.isZero) re.toString
    else if (re.isZero) im.toString + "i"
    else re.toString + (if (im.isNegative) "-" + (-im).toString else "+" + im.toString) + "i"


  //TODO-untested
  def toPoint = new Point2D(re, im)
}

//TODO untested
object ComplexLike{

  val realTag = implicitly[ClassTag[Real]]
  val real64Tag = implicitly[ClassTag[Real64]]

  val complexTag = implicitly[ClassTag[Complex]]
  val complex64Tag = implicitly[ClassTag[Complex64]]

  def unit[A <: ComplexLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case complexTag => Complex.unit
    case complex64Tag => Complex64.unit
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def zero[A <: ComplexLike[A]: ClassTag] = (implicitly[ClassTag[A]] match {
    case complexTag => Complex.zero
    case complex64Tag => Complex64.zero
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def polar[A <: RealLike[A]](angle: A, magnitude: A) = new ComplexLike[A](magnitude * calculus.cos(angle), magnitude * calculus.sin(angle))
}

object Complex extends Field[Complex] {

  def zero = new Complex(Real.zero, Real.zero)
  def unit = new Complex(Real.unit, Real.zero)
  def apply(re: Real, im: Real) = new Complex(re, im)
  def apply(re: Real) = new Complex(re, Real.zero)
  def unapply(that: Complex) = Some(that.Re, that.Im)

  def polar(angle: Real, magnitude: Real) = new Complex(magnitude * calculus.cos(angle), magnitude * calculus.sin(angle))
}

object Complex64 extends Field[Complex64] {

  def zero = new Complex64(Real64.zero, Real64.zero)
  def unit = new Complex64(Real64.unit, Real64.zero)
  def apply(re: Real64, im: Real64) = new Complex64(re, im)
  def apply(re: Real64) = new Complex64(re, Real64.zero)
  def unapply(that: Complex64) = Some(that.Re, that.Im)

  def polar(angle: Real64, magnitude: Real64) = new Complex64(magnitude * calculus.cos(angle), magnitude * calculus.sin(angle))
}

	