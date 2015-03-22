package smathla.algebra.structures.impl.integer

import org.scalatest.FreeSpecLike
import smathla.algebra.definitions._

import scala.util.Random

class IntegerInfTest extends FreeSpecLike {

  val gen = new Random()

  "IntegerInf" - {
    "method: +" in {
      for (i <- 0 to 10) {
        val a = gen.nextInt()
        val b = gen.nextLong()
        assert(IntegerInf(a) + IntegerInf(b) == IntegerInf(a + b), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for (i <- 0 to 10) {
        val a = BigInt(gen.nextInt())
        val b = BigInt(gen.nextLong())
        assert(IntegerInf(a) * IntegerInf(b) == IntegerInf(a * b), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for (i <- 0 to 10) {
        val a = gen.nextInt()
        val b = gen.nextLong()
        assert(IntegerInf(a) - IntegerInf(b) == IntegerInf(a - b), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for (i <- 0 to 10) {
        val a = gen.nextInt()
        val b = gen.nextLong()
        assert(IntegerInf(a) / IntegerInf(b) == IntegerInf(a / b), s"Error with: $a and $b")
      }
    }
    "method: %" in {
      for (i <- 0 to 10) {
        val a = gen.nextInt()
        val b = gen.nextLong()
        assert(IntegerInf(a) % IntegerInf(b) == IntegerInf(a % b), s"Error with: $a and $b")
      }
    }

    "method: gcd" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextLong()
        val answer = IntegerInf(a).gcd(IntegerInf(b))
        assert((answer | IntegerInf(a)) && (answer | IntegerInf(b)), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      assert(!IntegerInf(-1).isPositive, "-1")
      assert(!IntegerInf(0).isPositive, "0")
      assert(IntegerInf(1).isPositive, "1")
    }
    "method: isNegative" in {
      assert(IntegerInf(-1).isNegative, "-1")
      assert(!IntegerInf(0).isNegative, "0")
      assert(!IntegerInf(1).isNegative, "1")
    }
    "method: compareTo" in {
      assert(IntegerInf(-1).compareTo(IntegerInf(1)) === Lower)
      assert(IntegerInf(1).compareTo(IntegerInf(-1)) === Higher)
      assert(IntegerInf(1).compareTo(IntegerInf(1)) === Equal)
    }
    "method: unary_-" in {
      for (a <- 0 to 10) {
        val a = gen.nextLong()
        assert(-IntegerInf(a) == IntegerInf(-a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!IntegerInf(1).isZero)
      assert(!IntegerInf(-1).isZero)
      assert(IntegerInf(0).isZero)
    }
    "method: isUnit" in {
      assert(IntegerInf(1).isUnit)
      assert(!IntegerInf(0).isUnit)
      assert(!IntegerInf(-1).isUnit)
    }
  }
}
