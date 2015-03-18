package smathla.algebra.structures

/**
 * This class identifies element of algebraic structure that is called Field.
 * Field S has following properties:
 * 1) S is Domain.
 * 2) Multiplicative inverse exist for all elements in S\{0}. That is: For all a in S\{0} there exists b in S such that a*b = b*a = 1.
 */
trait FieldElem[A <: FieldElem[A]] extends DomainElem[A] {
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

trait Field[A <: FieldElem[A]] extends Domain[A]