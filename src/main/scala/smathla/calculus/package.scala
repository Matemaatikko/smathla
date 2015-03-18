package smathla

import smathla.algebra.concrete_structures.Real64.Real64
import smathla.algebra.concrete_structures.real.{Real, RealLike}

package object calculus {

  def sqrt[A <: RealLike[A]](a: A): A = (a match {
    case a: Real => Real(math.sqrt(a.toFloat).toFloat)
    case a: Real64 => Real64(math.sqrt(a.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def atan2[A <: RealLike[A]](a: A, b: A): A = ((a, b) match {
    case (a: Real, b: Real) => Real(math.atan2(a.toFloat, b.toFloat).toFloat)
    case (a: Real64, b: Real64) => Real64(math.atan2(a.toDouble, b.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def pow[A <: RealLike[A]](a: A, b: A): A = ((a, b) match {
    case (a: Real, b: Real) => Real(math.pow(a.toFloat, b.toFloat).toFloat)
    case (a: Real64, b: Real64) => Real64(math.pow(a.toDouble, b.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def sin[A <: RealLike[A]](a: A): A = (a match {
    case a: Real => Real(math.sin(a.toFloat).toFloat)
    case a: Real64 => Real64(math.sin(a.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def cos[A <: RealLike[A]](a: A): A = (a match {
    case a: Real => Real(math.cos(a.toFloat).toFloat)
    case a: Real64 => Real64(math.cos(a.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def tan[A <: RealLike[A]](a: A): A = (a match {
    case a: Real => Real(math.tan(a.toFloat).toFloat)
    case a: Real64 => Real64(math.tan(a.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def log[A <: RealLike[A]](a: A): A = (a match {
    case a: Real => Real(math.log(a.toFloat).toFloat)
    case a: Real64 => Real64(math.log(a.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

  def exp[A <: RealLike[A]](a: A): A = (a match {
    case a: Real => Real(math.exp(a.toFloat).toFloat)
    case a: Real64 => Real64(math.exp(a.toDouble))
    case _ => new NumberFormatException("implementation missing")
  }).asInstanceOf[A]

}
