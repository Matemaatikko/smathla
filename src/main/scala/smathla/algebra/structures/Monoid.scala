package smathla.algebra.structures

/**
 * This class identifies element of algebraic structure that is called monoid.
 * Monoid S has following properties:
 * 1) It is closed under binary operation (In this case operation is: +)
 * 2) Binary operation is associative: For all a,b,c in S: (a + b) + c = a + (b + c)
 * 3) It has neutral element: e. For all a in S: a + e = e + a = a
 */


trait MonoidElem[A <: MonoidElem[A]] extends SemiGroupElem[A] {
  this: A =>
  /**
   * Test if this is zero element of monoid.
   */
  def isZero: Boolean
}

trait Monoid[A <: MonoidElem[A]] extends SemiGroup[A]{

  /**
   * Returns identity element of monoid
   */
  def zero: A
}