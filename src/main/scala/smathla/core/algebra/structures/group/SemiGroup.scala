package smathla.core.algebra.structures.group

/**
 * This class identifies element of algebraic structure that called SemiGroup.
 * SemiGroup S has following properties:
 * 1) Its closed under binary operation (In this case operation is: +)
 * 2) Binary operation is associative: For all a,b,c in S: (a + b) + c = a + (b + c)
 */
trait SemiGroupElem[A <: SemiGroupElem[A]] {

  /**
   * Returns sum of elements this and given element a.
   */
  def +(a: A): A

}

trait SemiGroup[A <: SemiGroupElem[A]]