package smathla.core.algebra.structures.impl.matrix

import shapeless.{AdditiveCollection, Sized, Nat}
import smathla.core.algebra.structures.impl.vector
import smathla.core.algebra.structures.ring.RingElem

//TODO untested
class Matrix[N <: Nat, M <: Nat, R <: RingElem[R]](val rows: Sized[Seq[vector.Vector[M, R]], N]){

  implicit def additiveCollection[T] = new AdditiveCollection[Seq[T]] {}
  private val unsized = rows.unsized

  val columnCount = unsized.head.size
  val rowCount = unsized.size

  private[matrix] def get(i: Int, j: Int) = unsized(i).vec(j)

  def apply(i: Int, j: Int): Option[R] = unsized(i)(j)

  /**
    * Multiply this matrix with given matrix.
    * Matrix multiplication is defined as following:
    * c_i_j = sum_{j = 0 until k} a_n_j*b_j_k
    */
  def *[K <: Nat](matrix: Matrix[M, K, R]): Matrix[N, K, R] = {
    new  Matrix[N, K, R](
      Sized.wrap(
        (0 until rowCount).map(
          rowNum => {
            new vector.Vector[K, R](
              Sized.wrap(
                (0 until matrix.columnCount).map(
                  columnNum => {
                    def value(i: Int) = get(rowNum, i)*matrix.get(i, columnNum)
                    (1 until columnCount).foldLeft(value(0))((sum, ind) => sum + value(ind))
                  }
                )
              )
            )
          }
        )
      )
    )
  }

  /**
    * Add given matrix to this matrix.
    * c_i_j = a_i_j + b_i_j
    */
  def +(matrix: Matrix[N, M, R]): Matrix[N, M, R] = {
    new  Matrix[N, M, R](
      Sized.wrap(
        (0 until rowCount).map(ind => rows.unsized(ind) + matrix.rows.unsized(ind))
      )
    )
  }

  def transpose: Matrix[M, N, R] = {
    new Matrix[M, N, R](
      Sized.wrap(
        (0 until columnCount).map(
          columnNum => {
            new vector.Vector[N, R](
              Sized.wrap(
                (0 until rowCount).map(
                  rowNum => {
                    get(columnNum, rowNum)
                  }
                )
              )
            )
          }
        )
      )
    )
  }

  def *(scalar: R): Matrix[N, M, R] = {
    new Matrix[N, M, R](
      Sized.wrap(
        unsized.map(_*scalar)
      )
    )
  }

  override def toString(): String = {
    unsized.foldLeft("[")((str, vector) => str + vector.toString) + "]"
  }

  override def equals(any: Any) = any match {
    case a: Matrix[N, M, R] => this.unsized.equals(a.unsized)
    case _                           => false
  }
}

object Matrix{
  implicit def matrixOps[N <: Nat, M <: Nat, R <: RingElem[R]](matrix: Matrix[N, M, R]) = new ShapelessMatrixOps[N, M, R](matrix)
}


class ShapelessMatrixOps[N <: Nat, M <: Nat, R <: RingElem[R]](private val matrix: Matrix[N, M, R]){

  implicit def additiveCollection[T] = new AdditiveCollection[Seq[T]] {}

  def map[R2 <: RingElem[R2]](function: (Int, Int) => R2) = {
    new  Matrix[N, M, R2](
      Sized.wrap(
        (0 until matrix.rowCount).map(
          rowNum => {
            new vector.Vector[M, R2](
              Sized.wrap(
                (0 until matrix.columnCount).map(
                  columnNum => {
                    function(rowNum, columnNum)
                  }
                )
              )
            )
          }
        )
      )
    )
  }

  def map[R2 <: RingElem[R2]](function: R => R2) = {
    new  Matrix[N, M, R2](
      Sized.wrap(matrix.rows.unsized.map(elem => elem.map(function(_))))
    )
  }

  def forall(function: R => Boolean): Boolean = matrix.rows.forall(_.forall(function))

  def forall(function: (Int, Int, R) => Boolean): Boolean =
    (0 until matrix.rowCount).forall(
      rowNum => (0 until matrix.columnCount).forall(
        columnNum => function(rowNum, columnNum, matrix.get(rowNum, columnNum))
      )
    )
}

