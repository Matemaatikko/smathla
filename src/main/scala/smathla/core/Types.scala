package smathla.core

import smathla.core.algebra.structures.RingElem
import smathla.core.algebra.structures.impl.complex.{Complex, ComplexLike, GaussianLike}
import smathla.core.algebra.structures.impl.integer
import smathla.core.algebra.structures.impl.matrix.NumType
import smathla.core.algebra.structures.impl.rational.RationalLike


object Types {
  type JavaInteger = java.lang.Integer

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

  type Matrix[N <: NumType, M <: NumType, R <: RingElem[R]] = smathla.core.algebra.structures.impl.matrix.MatrixLike[N, M, R]
  val Matrix = smathla.core.algebra.structures.impl.matrix.Matrix

  type SquareMatrixLike[N <: NumType, R <: RingElem[R]] = smathla.core.algebra.structures.impl.matrix.MatrixLike[N, N, R]
  val SquareMatrix = smathla.core.algebra.structures.impl.matrix.SquareMatrix
}
