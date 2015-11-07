package smathla.core.algebra.structures.group

/**
 * Abelian group is commutative group.
 */
trait AbelianElem[A <: AbelianElem[A]] extends GroupElem[A] {
  this: A =>
}

trait Abelian[A <: AbelianElem[A]] extends Group[A]

