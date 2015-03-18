package smathla.algebra.concrete_structures.matrix

import smathla.algebra.structures.RingElem
import smathla.utils.Table

/**
 * This class identifies general matrix,
 */
class Matrix[A <: RingElem[A]](val matrix: Table[A]) {

  val dimensions = (matrix.columnCount, matrix.rowCount)
  val (columnCount, rowCount) = dimensions

  def apply(i: Int, j: Int): A = matrix(i, j)

  def *(a: Matrix[A]): Matrix[A] = {
    require(columnCount == a.rowCount, s"Matrix multiplication is not defined for two matrix with dimensions: $dimensions and ${a.dimensions} ")
    val table = for (i <- 0 until rowCount)
    yield for (j <- 0 until a.columnCount)
      yield (1 until columnCount).foldLeft(this(i, 0) * a(0, j))((elem, k) => elem + this(i, k) * this(k, j))
    return new Matrix[A](Table(table.asInstanceOf[List[List[A]]]))
  }

  def +(a: Matrix[A]): Matrix[A] = {
    require(dimensions == a.dimensions, s"Matrix addition is not defined for two matrix with dimensions: $dimensions and ${a.dimensions} ")
    val table = for (i <- 0 until rowCount)
    yield {
      for (k <- 0 until columnCount)
      yield this(i, k) + a(i, k)
    }
    return new Matrix[A](Table(table.asInstanceOf[List[List[A]]]))
  }

  def unary_- : Matrix[A] = {
    val table = for (i <- 0 until rowCount)
    yield for (k <- 0 until columnCount)
      yield -this(i, k)
    return new Matrix[A](Table(table.asInstanceOf[List[List[A]]]))
  }

  override def equals(that: Any) = that match {
    case a: Matrix[A] => a.matrix == matrix
    case _ => false
  }

  override def toString(): String = {
    var result = "["
    for (a <- 0 until rowCount) {
      result += "["
      for (b <- 0 until columnCount) {
        if (b == columnCount - 1) result += this(a, b)
        else result += this(a, b) + ","
      }
      result += "]"
      if (a != rowCount - 1) result += System.lineSeparator()
    }
    return result + "]"
  }

  def transpose: Matrix[A] = {
    val table = for (i <- 0 until columnCount)
    yield for (k <- 0 until rowCount)
      yield this(i, k)
    return new Matrix[A](Table(table.asInstanceOf[List[List[A]]]))
  }

  def *(a: A): Matrix[A] = Matrix(matrix.map(a * _))
}

object Matrix {

  def apply[A <: RingElem[A]](mat: Table[A]) = new Matrix[A](mat)
}