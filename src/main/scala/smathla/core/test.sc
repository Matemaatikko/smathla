
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import smathla.core.Types._
import smathla.core._

import smathla.core.algebra.structures.impl.matrix._

new Matrix[`2`, `3`, Integer](Seq(1, 1, 1, 1, 1, 1))* new Matrix[`3`, `1`, Integer](Seq(1, 1, 1))