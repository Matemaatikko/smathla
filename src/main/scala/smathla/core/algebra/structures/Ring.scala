package smathla.core.algebra.structures

import smathla.core.Types
import Types._

/**
 * This class identifies element of algebraic structure that is called Ring.
 * Ring S has following properties:
 * 1) S is abelian group.
 * 2) Multiplicative binary operator is associative: For all a,b,c in S: (a*b)*c = a*(b*c)
 * 3) Multiplication distributes over addition:
 * a * (b + c) = (a * b) + (a * c)
 * (a + b) * c = (a * c) + (b * c)
 * 4) S has multiplicative identity element u. For all a in S: u*a = a*u = a.
 *
 */
trait RingElem[A <: RingElem[A]] extends AbelianElem[A] {
  this: A =>

  /**
   * Returns multiplication between this and given element a.
   */
  def *(a: A): A

  /**
   * Tests if this is Unit element of Ring.
   */
  def isUnit: Boolean

}

trait Ring[A <: RingElem[A]] extends Abelian[A]{

  def unit: A

}