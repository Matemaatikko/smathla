package smathla

import smathla.algebra.structures.impl.complex.{GaussianLike, ComplexLike}
import smathla.algebra.structures.impl.integer
import smathla.algebra.structures.impl.integer.{IntegerInf, Integer64}
import smathla.algebra.structures.impl.rational.RationalLike
import smathla.algebra.structures.impl.real.{Real, Real64}
import smathla.calculus.elementary.ElementaryModifierLike

object Types {
  type JavaInteger = java.lang.Integer

  type Rational = RationalLike[integer.Integer]
  type Rational64 = RationalLike[Integer64]
  type RationalInf = RationalLike[IntegerInf]

  type Complex = ComplexLike[Real]
  type Complex64 = ComplexLike[Real64]

  type Gaussian = GaussianLike[integer.Integer]
  type Gaussian64 = GaussianLike[Integer64]

  type ElementaryModifier = ElementaryModifierLike[Real64]
}
