
package smathla.core.algebra.structures.impl.matrix

import smathla.core.Types._
import smathla.core.algebra.structures.RingElem

import scala.reflect.runtime.universe._

class SquareMatrix[N <: NumType: TypeTag, R <: RingElem[R]](private val matrix: SquareMatrixLike[N, R]) extends RingElem[SquareMatrix[N, R]] {

  val size = matrix.rowCount

  override def +(a: SquareMatrix[N, R]): SquareMatrix[N, R] = this.matrix + a.matrix
  override def *(a: SquareMatrix[N, R]): SquareMatrix[N, R] = this.matrix.*[N](a.matrix)

  override def unary_- = matrix.map((_,_, r) => -r)

  def isZero: Boolean = matrix.forall(_.isZero)

  def isUnit: Boolean = {
    (0 until matrix.elements.length).forall(ind => ((ind/size == ind%size)&& matrix.elements(ind).isUnit) || matrix.elements(ind).isZero)
  }

  def determinant: R = ???

}

object SquareMatrix {

  def apply[N <: NumType: TypeTag, R <: RingElem[R]](elements: Seq[R]): SquareMatrix[N, R] = new MatrixLike[N, N, R](elements)

  //TODO tools for square matrixes

  def fillDiagonal = ???

  implicit def matrix2square[N <: NumType: TypeTag, R <: RingElem[R]](matrix: SquareMatrixLike[N, R]): SquareMatrix[N, R] = new SquareMatrix[N, R](matrix)

}

