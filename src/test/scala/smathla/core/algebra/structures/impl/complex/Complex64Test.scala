package smathla.core.algebra.structures.impl.complex

import org.scalatest.FreeSpecLike
import smathla.core.Types
import Types.Complex64
import smathla.core.algebra.structures.impl.complex.Complex64
import smathla.core.algebra.structures.impl.real.{Real64, Real}
import smathla.calculus

import scala.util.Random

class Complex64Test extends FreeSpecLike {

  val gen = new Random()

  def nextComplex() = new Complex64(new Real64(gen.nextDouble()), new Real64(gen.nextDouble()))

  "Complex64" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        assert(a+b == Complex64(a.Re + b.Re, a.Im + b.Im), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        assert(a*b== Complex64(a.Re*b.Re - a.Im*b.Im, a.Re*b.Im + b.Re*a.Im), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        assert(a-b == Complex64(a.Re - b.Re, a.Im - b.Im), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 10){
        val a = nextComplex()
        val b = nextComplex()
        val prod = a*b.conjugate
        assert(a/b == Complex64(prod.Re/b.norm, prod.Im/b.norm), s"Error with: $a and $b")
      }
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = nextComplex()
        assert(-a == Complex64(-a.Re, -a.Im), s"Error with: $a")
      }
    }
    "method: unary_~" in {
      for(a <- 0 to 10){
        val a = nextComplex()
        assert(~a == a.conjugate/Complex64(a.norm), s"Error with: $a")
      }
    }
    "method: norm" in {
      for(a <- 0 to 10){
        val a = nextComplex()
        assert(a.norm === a.Re*a.Re + a.Im*a.Im, s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(Complex64.zero.isZero)
      assert(!Complex64(Real64(1), Real64(0)).isZero)
      assert(!Complex64(Real64(0), Real64(1)).isZero)
      assert(Complex64(Real64(0), Real64(0)).isZero)
    }
    "method: isUnit" in {
      assert(Complex64.unit.isUnit)
      assert(Complex64(Real64(1), Real64(0)).isUnit)
      assert(!Complex64(Real64(0), Real64(1)).isUnit)
      assert(!Complex64(Real64(0), Real64(0)).isUnit)
      assert(!Complex64(Real64(-1), Real64(0)).isUnit)
      assert(!Complex64(Real64(-1), Real64(-1)).isUnit)
    }
    "create with polar coordinates" in {
      var a = Complex64.polar(calculus.PI64, Real64(3))
      assert(a === Complex64(-3.0, 0.0))
      a = Complex64.polar(calculus.PI64/Real64(2), Real64(3))
      assert(a === Complex64(0.0, 3.0))
      a = Complex64.polar(2.0*calculus.PI64, Real64(3))
      assert(a === Complex64(3.0, 0.0))
    }
  }

}
