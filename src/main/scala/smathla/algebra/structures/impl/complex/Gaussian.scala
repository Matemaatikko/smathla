package smathla.algebra.structures.impl.complex

import smathla.Types.{Gaussian64, Gaussian}
import smathla.algebra.structures.impl.integer.{Integer, IntegerLike, Integer64}
import smathla.algebra.structures.{Euclidean, EuclideanElem}

import scala.reflect.ClassTag


case class GaussianLike[A <: IntegerLike[A]: ClassTag](private val re: A, private val im: A) extends EuclideanElem[GaussianLike[A]] {

  override def +(a: GaussianLike[A]) = new GaussianLike[A](re + a.Re, im + a.Im)
  override def unary_- = new GaussianLike[A](-re, -im)

  override def isUnit = re.isUnit && im.isZero
  override def isZero = re.isZero && im.isZero

  override def div(gaussian: GaussianLike[A]): (GaussianLike[A], GaussianLike[A]) = {
    val a = re
    val b = im

    val c = gaussian.re
    val d = gaussian.im

    val q = c*c + d*d
    val r0 = a*c + b*d
    val s0 = b*c - a*d

    val m = r0/q + (if((r0 % q) >= q/(IntegerLike.unit[A]+IntegerLike.unit[A])) IntegerLike.unit[A] else IntegerLike.zero[A])
    val n = s0/q + (if((s0 % q) >= q/(IntegerLike.unit[A]+IntegerLike.unit[A])) IntegerLike.unit[A] else IntegerLike.zero[A])

    (GaussianLike(m, n), this - GaussianLike(m, n)*gaussian)
  }

  override def *(a: GaussianLike[A]) = new GaussianLike[A](re * a.Re - im * a.Im, im * a.Re + re * a.Im)

  def Re = re
  def Im = im
  def conjugate = new GaussianLike[A](re, -im)
  def norm = re * re + im * im

  override def equals(that: Any) = that match {
    case null => false
    case a: GaussianLike[A] => a.im == this.im && a.re == this.re
    case _ => false
  }

  override def toString() =
    if (im.isUnit && re.isZero) "i"
    else if (im.isZero) re.toString
    else if (re.isZero) im.toString + "i"
    else re.toString + (if (im.isNegative) "-" + (-im).toString else "+" + im.toString) + "i"

}

object Gaussian extends Euclidean[Gaussian] {

  def unit = Gaussian(Integer.unit, Integer.zero)
  def zero =  Gaussian(Integer.zero, Integer.zero)

  def apply(re: Integer, im: Integer) = new Gaussian(re, im)
  def apply(re: Integer) = new Gaussian(re, Integer.zero)

  def unapply(that: Gaussian) = Some(that.Re, that.Im)
}


object Gaussian64 extends Euclidean[Gaussian64] {

  def unit = Gaussian64(Integer64.unit, Integer64.zero)
  def zero =  Gaussian64(Integer64.zero, Integer64.zero)

  def apply(re: Integer64, im: Integer64) = new Gaussian64(re, im)
  def apply(re: Integer64) = new Gaussian64(re, Integer64.zero)

  def unapply(that: Gaussian64) = Some(that.Re, that.Im)
}
