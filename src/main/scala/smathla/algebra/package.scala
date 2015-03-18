package smathla

import smathla.algebra.structures.EuclideanElem
import smathla.algebra.structures.impl.integer.IntegerLike

package object algebra {

  def gcd[A <: EuclideanElem[A]](a: A, b: A): A = a.gcd(b)

  /**
   * This executes given operation to itself 'times'-1 times.
   * Means: startValue as a: a op a op ... op a where a exists 'times' times.
   * NOTE: op must be associative that this method works!
   */
  def run[A, B <: IntegerLike[B]](times: B, startValue: A, assoc_op: (A, A) => A): Option[A]= { //TODO reimplement to return A.
    require(times.isPositive)
    val bin = times.toInt.toBinaryString.reverse //TODO add binary-array and replace this implementation
    var square: A = startValue
    var result = if (bin(0) == '1') Some(square) else None
    for (i <- 1 until bin.length()) {
      square = assoc_op(square, square)
      if (bin(i) == '1') {
        result = result match {
          case Some(a) => result.map(assoc_op(_, square))
          case None => Some(square)
        }
      }
    }
    result
  }

}
