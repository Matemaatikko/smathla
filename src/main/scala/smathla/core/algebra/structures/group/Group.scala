package smathla.core.algebra.structures.group

import smathla.core.algebra.structures.group.Monoid

/**
 * This class identifies element of algebraic structure that called Group.
 * Group S has following properties:
 * 1) Its closed under binary operation (In this case operation is: +)
 * 2) Binary operation is associative: For all a,b,c in S: (a + b) + c = a + (b + c)
 * 3) It has neutral element: e. For all a in S: a + e = e + a = a
 * 4) All elements has inverse. That means: For all a in S, there exists b, such that a + b = b + a = e, where e is neutral element.
 */
trait GroupElem[A <: GroupElem[A]] extends MonoidElem[A] {
  this: A =>

  /**
   * Returns additive inverse of element.
   */
  def unary_- : A

  def -(a: A) = this + -a
}

trait Group[A <: GroupElem[A]] extends Monoid[A]
