package smathla.core.algebra.structures.field

import smathla.core.algebra.structures.ring.{Domain, DomainElem}

/**
 * This class identifies element of algebraic structure that is called Division.
 * Division ring S has following properties:
 * 1) S is Domain.
 * 2) Multiplicative inverse exist for all elements in S\{0}. That is: For all a in S\{0} there exists b in S such that a*b = b*a = 1.
 */
trait DivisionRingElem[A <: DivisionRingElem[A]] extends DomainElem[A] {
  this: A =>
  /**
   * Returns multiplicative inverse of element.
   */
  def unary_~ : A

  def /(a: A) = {
    require(!a.isZero)
    this * ~a
  }
}

trait DivisionRing[A <: DivisionRingElem[A]] extends Domain[A]