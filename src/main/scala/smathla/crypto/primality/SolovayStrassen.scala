package smathla.crypto.primality

import smathla.core.algebra.structures.impl.integer.IntegerLike
import smathla.crypto.core.JacobiSymbol


object SolovayStrassen {
  import smathla.algebra._
  def test[A <: IntegerLike[A]](n: IntegerLike[A], iterations: IntegerLike[A]): Boolean = {
    var pointer = iterations
    while(!pointer.isZero){
      val a: A = ??? //Random integer 1 < a < n
      val d = n.gcd(a)
      if(d.isPositive && !d.isUnit) return false
      else {
        val j = JacobiSymbol(a, n)
        val x = run((n-IntegerLike.unit[A])/(IntegerLike.unit[A]+IntegerLike.unit[A]), a, (a1: A, a2: A) => a1*a2)
        if(j != x) return false
      }
      pointer = pointer + IntegerLike.unit[A]
    }
    return true
  }
}
