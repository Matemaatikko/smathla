package smathla.calculus.elementary

import smathla.algebra.structures.impl.real.RealLike
import smathla.calculus

/**
 * This class identifies elementary function: R -> R.
 */
abstract class ElementaryModifierLike[A <: RealLike[A]] {
  def +(f: ElementaryModifierLike[A]) = new Addition[A](this, f)
  def -(f: ElementaryModifierLike[A]) = new Subtraction[A](this, f)
  def *(f: ElementaryModifierLike[A]) = new Multiplication[A](this, f)
  def /(f: ElementaryModifierLike[A]) = new Division[A](this, f)
  def ^(f: ElementaryModifierLike[A]) = new Power[A](this, f)
}

abstract class ElementaryFunction[A <: RealLike[A]](f: ElementaryModifierLike[A]) extends  ElementaryModifierLike[A]
abstract class ElementaryOperator[A <: RealLike[A]](fst: ElementaryModifierLike[A], snd: ElementaryModifierLike[A]) extends  ElementaryModifierLike[A]

//ELEMENTARY OPERATORS

case class Multiplication[A <: RealLike[A]](fst: ElementaryModifierLike[A], snd: ElementaryModifierLike[A]) extends ElementaryOperator[A](fst, snd) {
  override def toString() = "(" + fst.toString + ")*(" + snd.toString + ")"
}
case class Division[A <: RealLike[A]](fst: ElementaryModifierLike[A], snd: ElementaryModifierLike[A]) extends ElementaryOperator[A](fst, snd) {
  override def toString() = "(" + fst.toString + ")/(" + snd.toString + ")"
}
case class Addition[A <: RealLike[A]](fst: ElementaryModifierLike[A], snd: ElementaryModifierLike[A]) extends ElementaryOperator[A](fst, snd) {
  override def toString() = "" + fst.toString + "+" + snd.toString + ""
}
case class Subtraction[A <: RealLike[A]](fst: ElementaryModifierLike[A], snd: ElementaryModifierLike[A]) extends ElementaryOperator[A](fst, snd) {
  override def toString() = "" + fst.toString + "-" + snd.toString + ""
}
case class Power[A <: RealLike[A]](base: ElementaryModifierLike[A], to: ElementaryModifierLike[A]) extends ElementaryOperator[A](base, to) {
  override def toString() = "(" + base.toString + ")^(" + to.toString + ")"
}

// VARIABLE AND CONSTANT
case class Variable[A <: RealLike[A]](v: Char) extends ElementaryModifierLike[A] {
  override def toString() = v.toString()
}
case class Constant[A <: RealLike[A]](d: A) extends ElementaryModifierLike[A] {
  override def toString() = d.toString()
}

//ELEMENTARY FUNCTIONS

case class Sin[A <: RealLike[A]](f: ElementaryModifierLike[A]) extends ElementaryFunction[A](f) {
  override def toString() = "sin(" + f.toString + ")"
}
case class Cos[A <: RealLike[A]](f: ElementaryModifierLike[A]) extends ElementaryFunction[A](f) {
  override def toString() = "cos(" + f.toString + ")"
}
case class Tan[A <: RealLike[A]](f: ElementaryModifierLike[A]) extends ElementaryFunction[A](f)  {
  override def toString() = "tan(" + f.toString + ")"
}
case class Exp[A <: RealLike[A]](f: ElementaryModifierLike[A]) extends ElementaryFunction[A](f) {
  override def toString() = "exp(" + f.toString + ")"
}
case class Log[A <: RealLike[A]](f: ElementaryModifierLike[A]) extends ElementaryFunction[A](f)  {
  override def toString() = "log(" + f.toString + ")"
}

/**
 * TODO
 * Functions to add:
 * inverse triconometrics
 * hyberbolic functions (also inverses)
 * log2, log10 and logN funktiot
 *
 */
