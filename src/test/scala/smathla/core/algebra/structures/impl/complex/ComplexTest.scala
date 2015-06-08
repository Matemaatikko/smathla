package smathla.core.algebra.structures.impl.complex

import org.scalatest.FreeSpecLike
import smathla.core.Types
import Types.Complex
import smathla.core.algebra.structures.impl.real.Real
import smathla.calculus

import scala.util.Random

class ComplexTest extends FreeSpecLike {

  val gen = new Random()

  def nextComplex() = new Complex(new Real(gen.nextFloat()), new Real(gen.nextFloat()))

  "Complex" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        assert(a+b == Complex(a.Re + b.Re, a.Im + b.Im), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        assert(a*b== Complex(a.Re*b.Re - a.Im*b.Im, a.Re*b.Im + b.Re*a.Im), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        assert(a-b == Complex(a.Re - b.Re, a.Im - b.Im), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        val prod = a*b.conjugate
        assert(a/b == Complex(prod.Re/b.norm, prod.Im/b.norm), s"Error with: $a and $b")
      }
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = nextComplex()
        assert(-a == Complex(-a.Re, -a.Im), s"Error with: $a")
      }
    }
    "method: unary_~" in {
      for(a <- 0 to 10){
        val a = nextComplex()
        assert(~a == a.conjugate/Complex(a.norm), s"Error with: $a")
      }
    }
    "method: norm" in {
      for(a <- 0 to 10){
        val a = nextComplex()
        assert(a.norm === a.Re*a.Re + a.Im*a.Im, s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(Complex.zero.isZero)
      assert(!Complex(Real(1), Real(0)).isZero)
      assert(!Complex(Real(0), Real(1)).isZero)
      assert(Complex(Real(0), Real(0)).isZero)
    }
    "method: isUnit" in {
      assert(Complex.unit.isUnit)
      assert(Complex(Real(1), Real(0)).isUnit)
      assert(!Complex(Real(0), Real(1)).isUnit)
      assert(!Complex(Real(0), Real(0)).isUnit)
      assert(!Complex(Real(-1), Real(0)).isUnit)
      assert(!Complex(Real(-1), Real(-1)).isUnit)
    }
    "create with polar coordinates" in {
      var a = Complex.polar(calculus.PI, Real(3))
      assert(a === Complex(-3f, 0f))
      a = Complex.polar(calculus.PI/Real(2), Real(3))
      assert(a === Complex(0f, 3f))
      a = Complex.polar(2f*calculus.PI, Real(3))
      assert(a === Complex(3f, 0f))
    }
  }

}
