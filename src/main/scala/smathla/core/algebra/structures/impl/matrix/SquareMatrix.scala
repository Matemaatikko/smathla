
package smathla.core.algebra.structures.impl.matrix

import shapeless.{Sized, Nat}
import smathla.core.algebra.structures.impl.vector.Vector
import smathla.core.algebra.structures.ring.RingElem

import scala.reflect.runtime.universe._

//TODO untested
class SquareMatrix[N <: Nat, R <: RingElem[R]](private val matrix: Matrix[N, N, R]) extends RingElem[SquareMatrix[N, R]] {

  val size = matrix.rowCount

  override def +(a: SquareMatrix[N, R]): SquareMatrix[N, R] = new SquareMatrix(this.matrix + a.matrix)
  override def *(a: SquareMatrix[N, R]): SquareMatrix[N, R] = new SquareMatrix(this.matrix.*[N](a.matrix))

  override def unary_- = new SquareMatrix(matrix.map[R]((elem: R) => -elem))

  def isZero: Boolean = matrix.forall(_.isZero)

  def isUnit: Boolean = {
    matrix.forall((row: Int, column: Int, elem: R) => if(row == column) elem.isUnit else elem.isZero)
  }

  def determinant: R = ??? //TODO implement

}

object SquareMatrix {

  def apply[N <: Nat, R <: RingElem[R]](rows: Sized[Seq[Vector[N, R]], N]) = new SquareMatrix(new Matrix[N, N, R](rows))

}

