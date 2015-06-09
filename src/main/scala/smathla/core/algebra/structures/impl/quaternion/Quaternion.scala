package smathla.core.algebra.structures.impl.quaternion

import smathla.calculus
import smathla.core.algebra.structures.{DivisionRingElem, Field}
import smathla.core.algebra.structures.impl.real.{RealLike}


//TODO all untested
class Quaternion[R <: RealLike[R]](r: R, i: R, j: R, k: R) extends DivisionRingElem[Quaternion[R]] {

  override def *(a: Quaternion[R]) =
    new Quaternion[R](
      r*a.r - i*a.i - j*a.j - k*a.k,
      r*a.i + i*a.r + j*a.k - k*a.j,
      r*a.j - i*a.k + j*a.r + k*a.i,
      r*a.k + i*a.j - j*a.i + k*a.r
    )
  override def +(a: Quaternion[R]) = new Quaternion[R](a.r + r, a.i + i, a.j + j, a.k + k)
  override def unary_- = new Quaternion[R](-r, -i, -j, -k)
  override def unary_~ = {
    val n = norm
    this*(~(n*n))
  }

  override def toString() = s"$r + $i i + $j j + $k k"
  override def isZero = r.isZero && i.isZero && j.isZero && k.isZero
  override def isUnit = r.isUnit && i.isZero && j.isZero && k.isZero

  def *(a: R) = new Quaternion[R](a*r, a*i, a*j, a*k)
  def conjugate = new Quaternion[R](r, -i, -j, -k)

  def norm: R = calculus.sqrt(r*r + i*i + j*j + k*k)

  override def equals(any: Any) = any match {
    case q: Quaternion[R] => r == q.r && i == q.i && j == q.j && k == q.k
    case _ => false
  }
}
