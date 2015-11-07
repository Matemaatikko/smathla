package smathla.core.algebra.structures.impl.vector

import shapeless.{AdditiveCollection, Nat, Sized}
import smathla.core.algebra.definitions.Metrics
import smathla.core.algebra.structures.ring.RingElem

//TODO untested
class Vector[N <: Nat, R <: RingElem[R]](val vector: Sized[Seq[R], N]){

  implicit def additiveCollection[T] = new AdditiveCollection[Seq[T]] {}
  val vec = vector.unsized
  val size = vector.unsized.size

  private[smathla] def get(i: Int) = vec(i)
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

  def -(vector: Vector[N, R]): Vector[N, R] = this + -vector

  def unary_- = new VectorOps(this).map(-_)

  /**
    * Dot product (scalar product) of 2 vector:
    * a_1*b_1 + ... + a_n*b_n
    */
  def *(vector: Vector[N, R]): R = {
    var ind = 0
    vec.tail.foldLeft(vec.head)((sum, elem) => {
      val part = vector.vec(ind)
      ind += 1
      sum + elem*part
    })
  }

  def *(scalar: R): Vector[N, R] = {
    new Vector[N, R](Sized.wrap[Seq[R], N](vec.map(_*scalar)))
  }

  override def toString: String = (1 until size).foldLeft("["+get(0))((str, m) => str + ", "+this.get(m)) + "]"

  override def equals(any: Any) = any match {
    case a: Vector[N, R] => this.vec.equals(a.vec)
    case _               => false
  }
}

object Vector{
  implicit def vectorOps[N <: Nat, R <: RingElem[R]](vector: Vector[N, R]) = new VectorOps[N, R](vector)
}

class VectorOps[N <: Nat, R <: RingElem[R]](private val vector: Vector[N, R]) {
  implicit def additiveCollection[T] = new AdditiveCollection[Seq[T]] {}
  def map[R2 <: RingElem[R2]](function: R => R2) = {
    new Vector[N, R2](Sized.wrap(vector.vector.unsized.map(function(_))))
  }
  def forall(function: R => Boolean) = vector.vector.forall(function)
}