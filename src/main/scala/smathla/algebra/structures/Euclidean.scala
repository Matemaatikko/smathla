package smathla.algebra.structures

/**
 * This class identifies element of algebraic structure that is called Euclidean domain.
 * Euclidean domain S has following properties:
 * 1) S is integral domain.
 * 2) For S there exists function f: S\{0}-> N such that for all a,b in S and b != 0, we can write a = q*b+r for some q, r in S with r = 0 or f(r) < f(b).
 */
trait EuclideanElem[A <: EuclideanElem[A]] extends DomainElem[A] {
  this: A =>
  /**
   * Method returns quantity and remainder (q, r) when this is divided by given element a.
   * post condition:	n(a) > n(r) || r == zero for some euclidean function n.
   */
  def div(a: A): (A, A)

  /**
   * Returns gcd of this and given parameter a.
   */
  def gcd(a: A): A = {
    if (a.isZero) return this
    else return a.gcd(this.div(a)._2)
  }
}

trait Euclidean[A <: EuclideanElem[A]] extends Domain[A]