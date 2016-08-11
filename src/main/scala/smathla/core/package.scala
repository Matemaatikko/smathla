package smathla

import shapeless.{Succ, Nat}
import smathla.core.algebra.structures.impl.complex.{GaussianLike, ComplexLike}
import smathla.core.algebra.structures.impl.rational.RationalLike
import smathla.core.algebra.structures.ring.RingElem

package object core {
  type JavaInteger = java.lang.Integer

  type `1` = Nat._1
  type `2` = Nat._2
  type `3` = Nat._3
  type `4` = Nat._4
  type `5` = Nat._5
  type `6` = Nat._6
  type `7` = Nat._7
  type `8` = Nat._8
  type `9` = Nat._9
  type `10` = Nat._10
  type `11` = Nat._11
  type `12` = Nat._12
  type `13` = Nat._13
  type `14` = Nat._14
  type `15` = Nat._15
  type `16` = Nat._16
  type `17` = Nat._17
  type `18` = Nat._18
  type `19` = Nat._19
  type `20` = Nat._20
  type `21` = Nat._21
  type `22` = Nat._22
  type `23` = Succ[`22`]
  type `24` = Succ[`23`]
  type `25` = Succ[`24`]

  //Rational

  type Rational = RationalLike[Integer]
  val Rational = smathla.core.algebra.structures.impl.rational.Rational

  type Rational64 = RationalLike[Integer64]
  val Rational64 = smathla.core.algebra.structures.impl.rational.Rational64

  type RationalInf = RationalLike[IntegerInf]
  val RationalInf = smathla.core.algebra.structures.impl.rational.RationalInf

  //Complex

  type Complex = ComplexLike[Real]
  val Complex = smathla.core.algebra.structures.impl.complex.Complex

  type Complex64 = ComplexLike[Real64]
  val Complex64 = smathla.core.algebra.structures.impl.complex.Complex64

  type Gaussian = GaussianLike[Integer]
  val Gaussian = smathla.core.algebra.structures.impl.complex.Gaussian

  type Gaussian64 = GaussianLike[Integer64]
  val Gaussian64 = smathla.core.algebra.structures.impl.complex.Gaussian64

  //Integer

  type Integer = smathla.core.algebra.structures.impl.integer.Integer
  val Integer =  smathla.core.algebra.structures.impl.integer.Integer

  type Integer64 = smathla.core.algebra.structures.impl.integer.Integer64
  val Integer64 =  smathla.core.algebra.structures.impl.integer.Integer64

  type IntegerInf = smathla.core.algebra.structures.impl.integer.IntegerInf
  val IntegerInf =  smathla.core.algebra.structures.impl.integer.IntegerInf

  //Real

  type Real = smathla.core.algebra.structures.impl.real.Real
  val Real = smathla.core.algebra.structures.impl.real.Real

  type Real64 = smathla.core.algebra.structures.impl.real.Real64
  val Real64 = smathla.core.algebra.structures.impl.real.Real64

  //Matrix

  type Vector[N <: Nat, R <: RingElem[R]] = smathla.core.algebra.structures.impl.vector.Vector[N, R]
  val Vector = smathla.core.algebra.structures.impl.vector.Vector

  val RealVector = smathla.core.geometry.RealVector

  type Matrix[N <: Nat, M <: Nat, R <: RingElem[R]] = smathla.core.algebra.structures.impl.matrix.Matrix[N, M, R]
  val Matrix = smathla.core.algebra.structures.impl.matrix.Matrix

  type SquareMatrix[N <: Nat, R <: RingElem[R]] = smathla.core.algebra.structures.impl.matrix.SquareMatrix[N, R]
  val SquareMatrix = smathla.core.algebra.structures.impl.matrix.SquareMatrix
}
