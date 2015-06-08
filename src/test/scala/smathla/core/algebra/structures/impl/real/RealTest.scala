package smathla.core.algebra.structures.impl.real


import org.scalatest.FreeSpecLike
import smathla.core.algebra.definitions._

import scala.util.Random

class RealTest extends FreeSpecLike {

  val gen = new Random()

  "Real" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = gen.nextFloat()
        val b = gen.nextFloat()
        assert(Real(a)+Real(b) == Real(a+b), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = gen.nextFloat()
        val b = gen.nextFloat()
        assert(Real(a)*Real(b) == Real(a*b), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = gen.nextFloat()
        val b = gen.nextFloat()
        assert(Real(a)-Real(b) == Real(a-b), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 1000){
        val a = gen.nextFloat()
        val b = gen.nextFloat()
        assert(Real(a)/Real(b) == Real(a/b), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      assert(!Real(-1).isPositive, "-1")
      assert(!Real(0).isPositive, "0")
      assert(Real(1).isPositive, "1")
    }
    "method: isNegative" in {
      assert(Real(-1).isNegative, "-1")
      assert(!Real(0).isNegative, "0")
      assert(!Real(1).isNegative, "1")
    }
    "method: compareTo" in {
      assert(Real(-1).compareTo(Real(1)) === Lower)
      assert(Real(1).compareTo(Real(-1)) === Higher)
      assert(Real(1).compareTo(Real(1)) === Equal)
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = gen.nextFloat()
        assert(-Real(a) == Real(-a), s"Error with: $a")
      }
    }
    "method: unary_~" in {
      for(a <- 0 to 10){
        val a = gen.nextFloat()
        assert(~Real(a) == Real(1f/a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!Real(1).isZero)
      assert(!Real(-1).isZero)
      assert(Real(0).isZero)
    }
    "method: isUnit" in {
      assert(Real(1).isUnit)
      assert(!Real(0).isUnit)
      assert(!Real(-1).isUnit)
    }
  }

}
