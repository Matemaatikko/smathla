
package smathla.core.algebra.structures.impl.matrix
/*

import smathla.core.algebra.structures.RingElem



/**
 * This class identifies square matrix over arbitrary ring.
 */
class SquareMatrix[A <: RingElem[A]](override val matrix: Table[A]) extends Matrix[A](matrix) with RingElem[SquareMatrix[A]] {

  override def +(a: SquareMatrix[A]): SquareMatrix[A] =
    new SquareMatrix[A]((this.asInstanceOf[Matrix[A]] + a.asInstanceOf[Matrix[A]]).matrix)

  override def *(a: SquareMatrix[A]): SquareMatrix[A] =
    new SquareMatrix[A]((this.asInstanceOf[Matrix[A]] * a.asInstanceOf[Matrix[A]]).matrix)

  override def unary_- =
    new SquareMatrix[A]((-this.asInstanceOf[Matrix[A]]).matrix)

  def isZero: Boolean = {
    for (i <- 0 until rowCount; j <- 0 until columnCount) {
      if (!this(i, j).isZero) false
    }
    true
  }

  def isUnit: Boolean = {
    for (i <- 0 until rowCount; j <- 0 until columnCount) {
      if (i != j && !this(i, j).isZero) false
      else if (i == j && !this(i, j).isUnit) false
    }
    true
  }

}

object SquareMatrix {

  def apply[A <: RingElem[A]](mat: Table[A]) = new SquareMatrix[A](mat)

}
*/
