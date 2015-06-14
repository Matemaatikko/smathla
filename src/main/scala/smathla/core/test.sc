
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import smathla.core.Types._

import smathla.core.algebra.structures.impl.matrix._

Complex(1.0, 2.0) match {
  case Complex(Real(1.0), Real(2.0)) => println("yes")
  case _ => println("no")
}