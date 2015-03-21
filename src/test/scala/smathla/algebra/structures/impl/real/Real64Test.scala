package smathla.algebra.structures.impl.Real64

import org.scalatest.FreeSpecLike
import smathla.algebra.definitions._
import smathla.algebra.structures.impl.real.Real64

import scala.util.Random


class Real64Test extends FreeSpecLike {

  val gen = new Random()

  "Real64" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = gen.nextDouble()
        val b = gen.nextDouble()
        assert(Real64(a)+Real64(b) == Real64(a+b), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = gen.nextDouble()
        val b = gen.nextDouble()
        assert(Real64(a)*Real64(b) == Real64(a*b), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = gen.nextDouble()
        val b = gen.nextDouble()
        assert(Real64(a)-Real64(b) == Real64(a-b), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 30){
        val a = gen.nextDouble()
        val b = gen.nextDouble()
        assert(Real64(a)/Real64(b) == Real64(a/b), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      assert(!Real64(-1).isPositive, "-1")
      assert(!Real64(0).isPositive, "0")
      assert(Real64(1).isPositive, "1")
    }
    "method: isNegative" in {
      assert(Real64(-1).isNegative, "-1")
      assert(!Real64(0).isNegative, "0")
      assert(!Real64(1).isNegative, "1")
    }
    "method: compareTo" in {
      assert(Real64(-1).compareTo(Real64(1)) === Lower)
      assert(Real64(1).compareTo(Real64(-1)) === Higher)
      assert(Real64(1).compareTo(Real64(1)) === Equal)
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = gen.nextDouble()
        assert(-Real64(a) == Real64(-a), s"Error with: $a")
      }
    }
    "method: unary_~" in {
      for(a <- 0 to 1000){
        val a = gen.nextDouble()
        assert(~Real64(a) == Real64(1f/a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!Real64(1).isZero)
      assert(!Real64(-1).isZero)
      assert(Real64(0).isZero)
    }
    "method: isUnit" in {
      assert(Real64(1).isUnit)
      assert(!Real64(0).isUnit)
      assert(!Real64(-1).isUnit)
    }
  }

}
