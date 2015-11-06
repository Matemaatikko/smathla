

import smathla.core.algebra.structures.impl.matrix._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import smathla.core.Types._
import smathla.core._
import shapeless._
import nat._
import syntax.sized._
val vector1 = new Vector(Sized(Integer(1), Integer(2)))
val vector2 = new Vector(Sized(Integer(1), Integer(2)))
val vector3 = new Vector(Sized(Integer(2)))
val vector4 = new Vector(Sized(Integer(2)))
val matrix1 = new ShapelessMatrix(Sized(vector1, vector2))
val matrix2 = new ShapelessMatrix(Sized(vector3, vector4))
matrix1.toString()
matrix2.toString()
val mult = matrix1*matrix2
mult.toString()