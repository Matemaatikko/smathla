package smathla.algebra.structures.impl.complex

import org.scalatest.FreeSpecLike
import smathla.Types.Gaussian
import smathla.algebra.structures.impl.integer.{IntegerInf, Integer}

import scala.util.Random


class GaussianTest extends FreeSpecLike {

  val gen = new Random()

  def nextGaussian(n : Int) = new Gaussian(new Integer(gen.nextInt(n)), new Integer(gen.nextInt(n)))
  def nextGaussian() = new Gaussian(new Integer(gen.nextInt()), new Integer(gen.nextInt()))

  "Gaussian" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = nextGaussian()
        val b = nextGaussian()
        assert(a+b == Gaussian(a.Re + b.Re, a.Im + b.Im), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = nextGaussian()
        val b = nextGaussian()
        assert(a*b== Gaussian(a.Re*b.Re - a.Im*b.Im, a.Re*b.Im + b.Re*a.Im), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = nextGaussian()
        val b = nextGaussian()
        assert(a-b == Gaussian(a.Re - b.Re, a.Im - b.Im), s"Error with: $a and $b")
      }
    }
    "method: div" in {
      for(i <- 0 to 100){
        val a = nextGaussian(10000)
        val b = nextGaussian(10000)
        val (q, r) = a.div(b)
        assert(a == b*q +r, s"Error with: $a and $b")
        assert(b.norm > r.norm, s"Error in norms with: $a and $b")
      }
    }
    "method: gcd" in {
      val n = 1000
      for(i <- 0 to 10){
        val a = nextGaussian(n)
        val b = nextGaussian(n)
        val answer = a.gcd(b)
        assert((answer | a) && (answer | b), s"Error with: $a and $b")
      }
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = nextGaussian()
        assert(-a == Gaussian(-a.Re, -a.Im), s"Error with: $a")
      }
    }
    "method: norm" in {
      for(a <- 0 to 10){
        val a = nextGaussian()
        assert(a.norm == a.Re*a.Re + a.Im*a.Im, s"Error with: $a")
      }
    }
    "method: conjugate" in {
      for(a <- 0 to 10){
        val a = nextGaussian()
        assert(a.conjugate == Gaussian(a.Re, -a.Im), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(Gaussian.zero.isZero)
      assert(!Gaussian(Integer(1), Integer(0)).isZero)
      assert(!Gaussian(Integer(0), Integer(1)).isZero)
      assert(Gaussian(Integer(0), Integer(0)).isZero)
    }
    "method: isUnit" in {
      assert(Gaussian.unit.isUnit)
      assert(Gaussian(Integer(1), Integer(0)).isUnit)
      assert(!Gaussian(Integer(0), Integer(1)).isUnit)
      assert(!Gaussian(Integer(0), Integer(0)).isUnit)
      assert(!Gaussian(Integer(-1), Integer(0)).isUnit)
      assert(!Gaussian(Integer(-1), Integer(-1)).isUnit)
    }
  }

}
