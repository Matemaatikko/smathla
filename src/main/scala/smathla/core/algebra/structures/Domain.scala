package smathla.core.algebra.structures

/**
 * This class identifies element of algebraic structure that is called Domain.
 * Domain S has following properties:
 * 1) S is Ring.
 * 2) For all a,b in S and a != 0 and b != 0: a*b != 0 and b*a != 0.
 */
trait DomainElem[A <: DomainElem[A]] extends RingElem[A] {
  this: A =>
}

trait Domain[A <: DomainElem[A]] extends Ring[A]