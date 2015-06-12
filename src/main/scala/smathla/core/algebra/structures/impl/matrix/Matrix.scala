package smathla.core.algebra.structures.impl.matrix

import smathla.core.algebra.structures.RingElem

import scala.reflect.runtime.universe._


sealed trait NumType
class `1` extends NumType
class `2` extends NumType
class `3` extends NumType
class `4` extends NumType
class `5` extends NumType
class `6` extends NumType
class `7` extends NumType
class `8` extends NumType
class `9` extends NumType
class `10` extends NumType
class `11` extends NumType
class `12` extends NumType
class `13` extends NumType
class `14` extends NumType
class `15` extends NumType
class `16` extends NumType
class `17` extends NumType
class `18` extends NumType
class `19` extends NumType
class `20` extends NumType
class `21` extends NumType
class `22` extends NumType
class `23` extends NumType
class `24` extends NumType
class `25` extends NumType
class `26` extends NumType
class `27` extends NumType
class `28` extends NumType
class `29` extends NumType
class `30` extends NumType
class `31` extends NumType
class `32` extends NumType
class `33` extends NumType
class `34` extends NumType
class `35` extends NumType


/**
 * TODO untested
 */
class Matrix[N <: NumType: TypeTag, M <: NumType: TypeTag, R <: RingElem[R]](val elements: Seq[R]){

  val rowCount = typeOf[N].typeSymbol.toString.substring(6).toInt
  val columnCount = typeOf[M].typeSymbol.toString.substring(6).toInt
  if(elements.length != rowCount*columnCount) throw new Exception(s"Invalid size of array: ${rowCount*columnCount} != ${elements.length}")

  def apply(i: Int, j: Int): R = elements(columnCount*i + j)

  def *[K <: NumType: TypeTag](matrix: Matrix[M, K, R]): Matrix[N, K, R] = {
    val list = for(
      n <- 0 until rowCount;
      k <- 0 until matrix.columnCount
    ) yield (1 until columnCount).foldLeft(this(n, 0)*matrix(0, k))((sum, ind) => sum + this(n, ind)*matrix(ind, k))
    new Matrix[N, K, R](list)
  }

  def +(matrix: Matrix[N, M, R]): Matrix[N, M, R] =
    new Matrix[N, M, R](for(i <- 0 until elements.size) yield elements(i) + matrix.elements(i))

  def *(r: R): Matrix[N, M, R] =
    new Matrix[N, M, R](for(i <- 0 until elements.size) yield elements(i)*r)

  def transpose: Matrix[M, N, R] =
    new Matrix[M, N, R](for(index <- 0 until elements.size) yield this(index%rowCount, index/rowCount))

  override def toString(): String = {
    (0 until rowCount).foldLeft("[")((str, n) => str + (1 until columnCount).foldLeft("["+this(n, 0))((str, m) => str + ", "+this(n, m)) + "]") + "]"
  }

  override def equals(any: Any): Boolean = any match {
    case a: Matrix[N, M, R] => a.elements == elements
    case _ => false
  }

  //MAPPING

  def map[B <: RingElem[B]](f: R => B): Matrix[N, M, B] =
    new Matrix[N, M, B](elements.map(f(_)))

  def mapRow(row: Int, f: R => R) =
    new Matrix[N, M, R](
      for(index <- 0 until elements.length)
        yield(
          if(index/columnCount == row) f(elements(index))
          else elements(index)
          )
    )

  def mapColumn(column: Int, f: R => R) =
    new Matrix[N, M, R](
      for(index <- 0 until elements.length)
        yield(
          if(index%columnCount == column) f(elements(index))
          else elements(index)
          )
    )

  //ROW OPERATIONS:

  /**
   * Swaps to rows.
   * There are some pre requirements:
   * 0 <= row1 < rowCount
   * 0 <= row2 < rowCount
   */
  def swap(row1: Int, row2: Int): Matrix[N, M, R] =
    new Matrix[N, M, R](
      for(index <- 0 until elements.length)
        yield(
          if(index/columnCount == row1) elements(row2*columnCount + index%columnCount)
          else if(index/columnCount == row2) elements(row1*columnCount + index%columnCount)
          else elements(index)
          )
    )

  /**
   * Multiplies all elements of given row with given element.
   * There are some pre requirements:
   * 0 <= row < rowCount
   */
  def mul(row: Int, value: R) = mapRow(row, _*value)

  /**
   * Adds first row to another.
   * There are some pre requirements:
   * 0 <= row < rowCount
   * 0 <= to < rowCount
   */
  def sum(row: Int, to: Int): Matrix[N, M, R] =
    new Matrix[N, M, R](
      for(index <- 0 until elements.length)
        yield(
          if(index/columnCount == to) elements(index) + elements(row*columnCount + index%columnCount)
          else elements(index)
          )
    )

}

object Matrix{

  def fill[N <: NumType: TypeTag, M <: NumType: TypeTag, R <: RingElem[R]](r: R): Matrix[N, M, R] = {
    val rowCount = typeOf[N].typeSymbol.toString.substring(6).toInt
    val columnCount = typeOf[M].typeSymbol.toString.substring(6).toInt
    val size = rowCount*columnCount
    new Matrix[N, M, R](Seq.fill(size)(r))
  }
}
