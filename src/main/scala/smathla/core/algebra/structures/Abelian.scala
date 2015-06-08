package smathla.core.algebra.structures


/**
 * This class identifies element of algebraic structure that called Abelian Group.
 * Abelian Group S has following properties:
 * 1) Its closed under binary operation (In this case operation is: +)
 * 2) Binary operation is associative: For all a,b,c in S: (a + b) + c = a + (b + c)
 * 3) It has neutral element: e. For all a in S: a + e = e + a = a
 * 4) All elements has inverse. That means: For all a in S, there exists b, such that a + b = b + a = e, where e is neutral element.
 * 5) Binary operator is commutative: For all a, b in S: a + b = b + a.
 */
trait AbelianElem[A <: AbelianElem[A]] extends GroupElem[A] {
  this: A =>
}

trait Abelian[A <: AbelianElem[A]] extends Group[A]

