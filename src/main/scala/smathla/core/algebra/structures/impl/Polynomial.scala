package smathla.core.algebra.structures.impl

import integer.Integer
import smathla.core.algebra.structures.ring.RingElem

case class Polynomial[E <: RingElem[E]] private(val map: Map[Integer, E]) extends RingElem[Polynomial[E]] {

  override def *(a: Polynomial[E]) = {
    var ca = Map[Integer, E]()
    for (i <- map; j <- a.map) {
      if (ca.contains(i._1 + j._1)) ca.updated(i._1 + j._1, ca(i._1 + j._1) + i._2 * j._2)
      else ca += i._1 + j._1 -> i._2 * j._2
    }
    Polynomial(ca)
  }

  override def +(a: Polynomial[E]) = {
    var ca = Map[Integer, E]()
    ca ++= map
    for (i <- a.map) {
      if (ca.contains(i._1)) ca.updated(i._1, ca(i._1) + i._2)
      else ca += i._1 -> i._2
    }
    Polynomial(ca)
  }

  override def unary_- = Polynomial(map.map(a => (a._1, -a._2)))

  override def toString() = map.foldLeft(z = "")((z, a) => if (z == "") a._2 + "x^" + a._1 else a._2 + "x^" + a._1 + "+" + z)
  override def isZero = (map.size == 1 && map.isDefinedAt(integer.Integer.zero) && map(integer.Integer.zero).isZero) || map.size == 0

  override def isUnit = map.size == 1 && map.isDefinedAt(integer.Integer.zero) && map(integer.Integer.zero).isUnit
}
