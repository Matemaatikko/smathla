package smathla.core

import smathla.core.algebra.structures.impl.complex.{ComplexLike, GaussianLike}
import smathla.core.algebra.structures.impl.integer
import smathla.core.algebra.structures.impl.rational.RationalLike

object Types {
  type JavaInteger = java.lang.Integer

  type Rational = RationalLike[Integer]
  type Rational64 = RationalLike[Integer64]
  type RationalInf = RationalLike[IntegerInf]

  type Complex = ComplexLike[Real]
  type Complex64 = ComplexLike[Real64]

  type Gaussian = GaussianLike[integer.Integer]
  type Gaussian64 = GaussianLike[Integer64]

  type Integer = smathla.core.algebra.structures.impl.integer.Integer
  type Integer64 = smathla.core.algebra.structures.impl.integer.Integer64
  type IntegerInf = smathla.core.algebra.structures.impl.integer.IntegerInf

  type Real = smathla.core.algebra.structures.impl.real.Real
  type Real64 = smathla.core.algebra.structures.impl.real.Real64
}
