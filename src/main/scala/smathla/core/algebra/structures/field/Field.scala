package smathla.core.algebra.structures.field

/**
  * Field is division ring where multiplication is commutative. (a*b == b*a)
  */

trait FieldElem[A <: FieldElem[A]] extends DivisionRingElem[A] {
  this: A =>
}

trait Field[A <: FieldElem[A]] extends DivisionRing[A]