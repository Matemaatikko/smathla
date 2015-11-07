

import smathla.core.algebra.structures.impl.matrix._
import smathla.core.algebra.structures.impl.vector
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import smathla.core.Types._
import smathla.core._
import shapeless._
import nat._
import syntax.sized._
val vector1 = new vector.Vector(Sized(Integer(1), Integer(2)))
val vector2 = new vector.Vector(Sized(Integer(1), Integer(2)))
val vector3 = new vector.Vector(Sized(Integer(2)))
val vector4 = new vector.Vector(Sized(Integer(2)))
val matrix1 = new Matrix(Sized(vector1, vector2))
val matrix2 = new Matrix(Sized(vector3, vector4))
matrix1.toString()
matrix2.toString()
val mult = matrix1*matrix2
mult.toString()