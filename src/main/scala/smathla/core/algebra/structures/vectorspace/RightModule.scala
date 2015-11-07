package smathla.core.algebra.structures.vectorspace

import smathla.core.algebra.structures.ring.RingElem

/**
  * A left R-module over ring R is defined by following axioms:
  *  Let 1R be identity of R and r, s in R and v, m in V.
  *  1. (x + y)*r == x*r + y*r
  *  2. x*(r + s) == x*r + x*s
  *  3. x*(s*r) == (x*s)*r
  *  4. x*1R == x
  */
trait RightModuleElem[R <: RingElem[R], V <: RightModuleElem[R, V]] {

  def +(vector: V): V
  def *(scalar: R): V
}

trait RightModule[F <: RingElem[F], V <: RightModuleElem[F, V]]
