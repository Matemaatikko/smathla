package smathla.calculus.elementary

import smathla.core.algebra.structures.impl.real.RealLike
import smathla.calculus

import scala.reflect.ClassTag

sealed abstract class Evaluation[+A]{
  final def fold[B >: A](a: Evaluation[B], op: (B, B) => B): Evaluation[B] =
    if(this == NonValue) NonValue
    else {
      val value = a.asInstanceOf[Value[B]].value
      a match {
        case Value(value2) => Value(op(value, value2))
        case _ => NonValue
      }
    }
  final def map[B](f: A => B): Evaluation[B] = this match{
    case Value(value) => Value[B](f(value))
    case NonValue => NonValue
  }
}
case object NonValue extends Evaluation[Nothing]
final case class Value[A](value: A) extends Evaluation[A]

case object Derivation{
  def derive[A <: RealLike[A]: ClassTag](function: ElementaryModifierLike[A])(implicit variable: Variable[A]): ElementaryModifierLike[A] = function match {
    case Multiplication(fst, snd) => derive(fst) * snd + fst * derive(snd)
    case Division(fst, snd) => (derive(fst) * snd - fst * derive(snd)) / (snd * snd)
    case Addition(fst, snd) => derive(fst) +  derive(snd)
    case Subtraction(fst, snd) => derive(fst) - derive(snd)
    case Power(base, to) => (base ^ to) * (to / base + derive(to) * Log(base))
    case a: Variable[A] if a == variable => new Constant[A](RealLike.unit[A])
    case a: Variable[A] if a != variable => new Constant[A](RealLike.zero[A])
    case _: Constant[A] => new Constant[A](RealLike.zero[A])

    case Sin(f) => Cos(f) * derive(f)
    case Cos(f) => new Constant[A](-RealLike.unit[A]) * Sin[A](f) * derive(f)
    case Tan(f) => (new Constant(RealLike.unit[A]) + (new Tan[A](f)*new Tan[A](f))) * derive(f)
    case Exp(f) => Exp(f) * derive(f)
    case Log(f) => derive(f) / f
  }
}

case object Evaluation{
  def evaluate[A <: RealLike[A]](function: ElementaryModifierLike[A]): Evaluation[A] = function match {
    case Multiplication(fst, snd) => evaluate(fst).fold(evaluate(snd), _*_)
    case Division(fst, snd) => evaluate(fst).fold(evaluate(snd), _/_)
    case Addition(fst, snd) => evaluate(fst).fold(evaluate(snd), _+_)
    case Subtraction(fst, snd) => evaluate(fst).fold(evaluate(snd), _-_)
    case Power(base, to) => evaluate(base).fold(evaluate(to), calculus.pow(_, _))
    case a: Variable[A]  => NonValue
    case Constant(c) => Value(c)

    case Sin(f) => evaluate(f).map(calculus.sin(_))
    case Cos(f) => evaluate(f).map(calculus.cos(_))
    case Tan(f) => evaluate(f).map(calculus.tan(_))
    case Exp(f) => evaluate(f).map(calculus.exp(_))
    case Log(f) => evaluate(f).map(calculus.log(_))
  }
}

case object Substitution{
  def substitute[A <: RealLike[A]](function: ElementaryModifierLike[A])( implicit value: A, variable: Variable[A]):  ElementaryModifierLike[A] = function match {
    case Multiplication(fst, snd) => Multiplication(substitute(fst), substitute(snd))
    case Division(fst, snd) => Division(substitute(fst), substitute(snd))
    case Addition(fst, snd) => Addition(substitute(fst), substitute(snd))
    case Subtraction(fst, snd) => Subtraction(substitute(fst), substitute(snd))
    case Power(base, to) => Power(substitute(base), substitute(to))
    case a: Variable[A] if a == variable  => Constant[A](value)
    case a: Variable[A] if a != variable  => a
    case c: Constant[A] => c

    case Sin(f) => Sin(substitute(f))
    case Cos(f) => Cos(substitute(f))
    case Tan(f) => Tan(substitute(f))
    case Exp(f) => Exp(substitute(f))
    case Log(f) => Log(substitute(f))
  }
}

case object Reduction{ //TODO reduction
  def reduce[A <: RealLike[A]](function: ElementaryModifierLike[A]): ElementaryModifierLike[A] = ???
}