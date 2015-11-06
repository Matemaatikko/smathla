package smathla.core.algebra.structures.impl.matrix

import shapeless.{AdditiveCollection, Sized, Nat}
import smathla.core.algebra.structures.RingElem

class ShapelessMatrix[N <: Nat, M <: Nat, R <: RingElem[R]](val rows: Sized[Seq[Vector[M, R]], N]){

  implicit def additiveCollection[T] = new AdditiveCollection[Seq[T]] {}
  private val unsized = rows.unsized

  val columnCount = unsized.head.size
  val rowCount = unsized.size

  private def get(i: Int, j: Int) = unsized(i).vec(j)

  def apply(i: Int, j: Int): Option[R] = unsized(i)(j)

  /**
    * Multiply this matrix with given matrix.
    * Matrix multiplication is defined as following:
    * (a)_n_m*(b)_m_k = (c)_n_k, where  c_i_j = sum_{j = 0 until k} a_n_j*b_j_k
    */
  def *[K <: Nat](matrix: ShapelessMatrix[M, K, R]): ShapelessMatrix[N, K, R] = {
    new  ShapelessMatrix[N, K, R](
      Sized.wrap(
        (0 until rowCount).map(
          rowNum => {
            new Vector[K, R](
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

  override def toString(): String = {
    (0 until rowCount).foldLeft("[")((str, n) => str + (1 until columnCount).foldLeft("["+this.get(n, 0))((str, m) => str + ", "+this.get(n, m)) + "]") + "]"
  }

/*  def +(matrix: MatrixLike[N, M, R]): MatrixLike[N, M, R] =
    new MatrixLike[N, M, R](for(i <- 0 until elements.size) yield elements(i) + matrix.elements(i))

  def *(r: R): MatrixLike[N, M, R] =
    new MatrixLike[N, M, R](for(i <- 0 until elements.size) yield elements(i)*r)

  def transpose: MatrixLike[N, M, R] =
    new MatrixLike[N, M, R](for(index <- 0 until elements.size) yield this(index%rowCount, index/rowCount))

  override def toString(): String = {
    (0 until rowCount).foldLeft("[")((str, n) => str + (1 until columnCount).foldLeft("["+this(n, 0))((str, m) => str + ", "+this(n, m)) + "]") + "]"
  }

  override def equals(any: Any): Boolean = any match {
    case a: MatrixLike[N, M, R] => a.elements == elements
    case _ => false
  }*/

}

class Vector[N <: Nat, R <: RingElem[R]](val vector: Sized[Seq[R], N]){

  implicit def additiveCollection[T] = new AdditiveCollection[Seq[T]] {}
  val vec = vector.unsized
  val size = vector.unsized.size

  def apply(i: Int): Option[R] =  if(vec.isDefinedAt(i)) Some(vec(i)) else None

  def +(vector: Vector[N, R]): Vector[N, R] = {
    var counter = 0
    new Vector[N, R](Sized.wrap[Seq[R], N](vec.map(
      (elem: R) => {
        val part = vector.vec(counter)
        counter += 1
        part + elem
      }
    )))
  }
}
