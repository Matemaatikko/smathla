package smathla.core

import smathla.calculus.elementary.ElementaryModifierLike
import smathla.core.algebra.structures.impl.complex.{ComplexLike, GaussianLike}
import smathla.core.algebra.structures.impl.integer
import smathla.core.algebra.structures.impl.integer.{Integer64, IntegerInf}
import smathla.core.algebra.structures.impl.rational.RationalLike
import smathla.core.algebra.structures.impl.real.{Real, Real64}

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
