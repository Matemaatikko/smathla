package smathla.utils

import scala.collection.mutable

abstract class AbstractTable[E](val a: Seq[Seq[E]]) {

  require(isLegal, "Given Array of arrays can't be 2-dimensional table!")

  private def isLegal: Boolean = {
    val columnCount = a(0).size
    a.forall(_.size == columnCount)
  }

  def columnCount = a(0).size

  def rowCount = a.size

  def apply(i: Int, j: Int) = a(i)(j)

  def map[B](f: E => B): AbstractTable[B]

  def mapRow(row: Int, f: E => E): AbstractTable[E]

  def mapColumn(column: Int, f: E => E): AbstractTable[E]

  def mapDiagonal(f: E => E): AbstractTable[E]
}

/**
 * Immutable 2-dimensional array.
 */
case class Table[E](override val a: List[List[E]]) extends AbstractTable[E](a) {
  def map[B](f: E => B): Table[B] = new Table(a.map(_.map(f)))

  def mapRow(row: Int, f: E => E): Table[E] = {
    new Table(
      (for (i <- 0 until rowCount)
      yield if (i == row) a(i).map(f) else a(i)).toList
    )
  }

  def mapColumn(column: Int, f: E => E): Table[E] = {
    new Table(
      (for (i <- 0 until rowCount)
      yield
        (for (j <- 0 until columnCount)
        yield if (j == column) f(a(i)(j)) else a(i)(j)).toList).toList
    )
  }

  /**
   * Diagonal means those positions where row-number is same as column-number.
   */
  def mapDiagonal(f: E => E): Table[E] = {
    new Table(
      (for (i <- 0 until rowCount)
      yield
        (for (j <- 0 until columnCount)
        yield if (j == i) f(a(i)(j)) else a(i)(j)).toList).toList
    )
  }
}

/**
 * mutable 2-dimensional array.
 */
case class MutableTable[E](override val a: mutable.Seq[mutable.Seq[E]]) extends AbstractTable[E](a) {

  def map[B](f: E => B): MutableTable[B] = new MutableTable(a.map(_.map(f)))

  def mapRow(row: Int, f: E => E): MutableTable[E] = {
    new MutableTable(
      (for (i <- 0 until rowCount)
      yield if (i == row) a(i).map(f) else a(i)).asInstanceOf[mutable.Seq[mutable.Seq[E]]]
    )
  }

  def mapColumn(column: Int, f: E => E): MutableTable[E] = {
    new MutableTable(
      (for (i <- 0 until rowCount)
      yield
        (for (j <- 0 until columnCount)
        yield if (j == column) f(a(i)(j)) else a(i)(j))).asInstanceOf[mutable.Seq[mutable.Seq[E]]]
    )
  }

  /**
   * Diagonal means those positions where row-number is same as column-number.
   */
  def mapDiagonal(f: E => E): MutableTable[E] = {
    new MutableTable(
      (for (i <- 0 until rowCount)
      yield
        (for (j <- 0 until columnCount)
        yield if (j == i) f(a(i)(j)) else a(i)(j))).asInstanceOf[mutable.Seq[mutable.Seq[E]]]
    )
  }

  def set(i: Int, j: Int, element: E) {
    a(i)(j) = element
  }
}

object Table {
  def toMutable[E](a: Table[E]) = MutableTable(a.a.asInstanceOf[mutable.Seq[mutable.Seq[E]]])

  def toImmutable[E](a: MutableTable[E]) = Table(a.a.asInstanceOf[List[List[E]]])
}