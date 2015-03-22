package smathla.algebra.structures.impl.rational

import org.scalatest.FreeSpecLike
import smathla.algebra.definitions._
import smathla.algebra.structures.impl.integer.Integer

import scala.util.Random

class RationalTest extends FreeSpecLike {

  val gen = new Random()

  "Rational" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        val c = gen.nextInt()
        val d = gen.nextInt()
        assert(Rational(a, b)+Rational(c, d) == Rational(a*d + b*c, b*d), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        val c = gen.nextInt()
        val d = gen.nextInt()
        assert(Rational(a, b)*Rational(c, d) == Rational(a*c, b*d), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        val c = gen.nextInt()
        val d = gen.nextInt()
        assert(Rational(a, b)-Rational(c, d) == Rational(a*d - b*c, b*d), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 100){
        val a = gen.nextInt()
        val b = gen.nextInt()
        val c = gen.nextInt()
        val d = gen.nextInt()
        assert(Rational(a, b)/Rational(c, d) == Rational(a*d, b*c), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      for(i <- 0 to 30){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Rational(a, b).isPositive == (a.signum*b.signum > 0), s"Error with: $a and $b")
      }
    }
    "method: isNegative" in {
      for(i <- 0 to 30){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Rational(a, b).isNegative == (a.signum*b.signum < 0), s"Error with: $a and $b")
      }
    }
    "method: compareTo" in {
      val n = 10000
      for(i <- 0 to 100){
        val a = gen.nextInt(n)
        val b = gen.nextInt(n)
        val c = gen.nextInt(n)
        val d = gen.nextInt(n)
        assert((Rational(a, b) < Rational(c, d)) == ((Rational(a, b) - Rational(c,d)).signum < Integer(0)), s"Error with: $a, $b, $c, $d")
      }
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(-Rational(a, b) == Rational(-a, b), s"Error with: $a")
      }
    }
    "method: unary_~" in {
      for(a <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(~Rational(a, b) == Rational(b, a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!Rational(1).isZero)
      assert(!Rational(-1).isZero)
      assert(Rational(0).isZero)
    }
    "method: isUnit" in {
      assert(Rational(1).isUnit)
      assert(!Rational(0).isUnit)
      assert(!Rational(-1).isUnit)
    }
  }

}
