package smathla.core.algebra.structures.impl.Rational64

import org.scalatest.FreeSpecLike
import smathla.core.algebra.structures.impl.integer.{Integer64, Integer}
import smathla.core.algebra.structures.impl.rational.Rational64

import scala.util.Random

class Rational64Test extends FreeSpecLike {

  val gen = new Random()

  "Rational64" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        val c = gen.nextLong()
        val d = gen.nextLong()
        assert(Rational64(a, b)+Rational64(c, d) == Rational64(a*d + b*c, b*d), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        val c = gen.nextLong()
        val d = gen.nextLong()
        assert(Rational64(a, b)*Rational64(c, d) == Rational64(a*c, b*d), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        val c = gen.nextLong()
        val d = gen.nextLong()
        assert(Rational64(a, b)-Rational64(c, d) == Rational64(a*d - b*c, b*d), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 100){
        val a = gen.nextLong()
        val b = gen.nextLong()
        val c = gen.nextLong()
        val d = gen.nextLong()
        assert(Rational64(a, b)/Rational64(c, d) == Rational64(a*d, b*c), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      for(i <- 0 to 30){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Rational64(a, b).isPositive == (a.signum*b.signum > 0), s"Error with: $a and $b")
      }
    }
    "method: isNegative" in {
      for(i <- 0 to 30){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Rational64(a, b).isNegative == (a.signum*b.signum < 0), s"Error with: $a and $b")
      }
    }
    "method: compareTo" in {
      for(i <- 0 to 100){
        val a = gen.nextInt()
        val b = gen.nextInt()
        val c = gen.nextInt()
        val d = gen.nextInt()
        assert((Rational64(a, b) < Rational64(c, d)) == ((Rational64(a, b) - Rational64(c,d)).signum < Integer64(0)), s"Error with: $a, $b, $c, $d")
      }
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(-Rational64(a, b) == Rational64(-a, b), s"Error with: $a")
      }
    }
    "method: unary_~" in {
      for(a <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(~Rational64(a, b) == Rational64(b, a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!Rational64(1).isZero)
      assert(!Rational64(-1).isZero)
      assert(Rational64(0).isZero)
    }
    "method: isUnit" in {
      assert(Rational64(1).isUnit)
      assert(!Rational64(0).isUnit)
      assert(!Rational64(-1).isUnit)
    }
  }
}
