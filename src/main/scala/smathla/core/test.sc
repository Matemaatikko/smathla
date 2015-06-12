
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import smathla.core.Types._

import smathla.core.algebra.structures.impl.matrix._


val a = Matrix.fill[`3`, `3`, Real](Real(1))

a.toString()
