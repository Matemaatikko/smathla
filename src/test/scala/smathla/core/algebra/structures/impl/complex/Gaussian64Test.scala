package smathla.core.algebra.structures.impl.complex

import java.lang.Integer

import org.scalatest.FreeSpecLike
import smathla.core.Types
import Types.Gaussian64
import smathla.core.algebra.structures.impl.complex.GaussianLike
import smathla.core.algebra.structures.impl.integer.{Integer64, Integer}

import scala.util.Random

class Gaussian64Test extends FreeSpecLike {

  val gen = new Random()

  def nextGaussian() = new Gaussian64(new Integer64(gen.nextLong()), new Integer64(gen.nextLong()))
  def nextGaussian(n: Int) = new Gaussian64(new Integer64(gen.nextInt(n)), new Integer64(gen.nextInt(n)))

  "Gaussian64" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = nextGaussian()
        val b = nextGaussian()
        assert(a+b == Gaussian64(a.Re + b.Re, a.Im + b.Im), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = nextGaussian()
        val b = nextGaussian()
        assert(a*b== Gaussian64(a.Re*b.Re - a.Im*b.Im, a.Re*b.Im + b.Re*a.Im), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = nextGaussian()
        val b = nextGaussian()
        assert(a-b == Gaussian64(a.Re - b.Re, a.Im - b.Im), s"Error with: $a and $b")
      }
    }
    "method: div" in {
      val n = 1000000
      for(i <- 0 to 10){
        val a = nextGaussian(n)
        val b = nextGaussian(n)
        val (q, r) = a.div(b)
        assert(a == b*q +r, s"Error with: $a and $b")
        assert(b.norm > r.norm, s"Error in norms with: $a and $b")
      }
    }
    "method: gcd" in {
      val n = 1000000
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
        assert(-a == Gaussian64(-a.Re, -a.Im), s"Error with: $a")
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
        assert(a.conjugate == Gaussian64(a.Re, -a.Im), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(Gaussian64.zero.isZero)
      assert(!Gaussian64(Integer64(1), Integer64(0)).isZero)
      assert(!Gaussian64(Integer64(0), Integer64(1)).isZero)
      assert(Gaussian64(Integer64(0l), Integer64(0l)).isZero)
    }
    "method: isUnit" in {
      assert(Gaussian64.unit.isUnit)
      assert(Gaussian64(Integer64(1), Integer64(0)).isUnit)
      assert(!Gaussian64(Integer64(0), Integer64(1)).isUnit)
      assert(!Gaussian64(Integer64(0), Integer64(0)).isUnit)
      assert(!Gaussian64(Integer64(-1), Integer64(0)).isUnit)
      assert(!Gaussian64(Integer64(-1), Integer64(-1)).isUnit)
    }
  }

}
