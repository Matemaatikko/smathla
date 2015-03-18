package smathla

import java.lang.{Integer => JavaInteger}

import smathla.algebra.concrete_structures.Real64.Real64
import smathla.algebra.concrete_structures.complex.{GaussianLike, ComplexLike}
import smathla.algebra.concrete_structures.integer.{Integer, IntegerInf, Integer64}
import smathla.algebra.concrete_structures.rational.RationalLike
import smathla.algebra.concrete_structures.real.Real

object Types {
  type JavaInteger = java.lang.Integer

  type Rational = RationalLike[Integer]
  type Rational64 = RationalLike[Integer64]
  type RationalInf = RationalLike[IntegerInf]

  type Complex = ComplexLike[Real]
  type Complex64 = ComplexLike[Real64]

  type Gaussian = GaussianLike[Integer]
  type Gaussian64 = GaussianLike[Integer64]

}
