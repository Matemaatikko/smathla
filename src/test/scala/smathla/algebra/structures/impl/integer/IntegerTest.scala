package smathla.algebra.structures.impl.integer

import org.scalatest.FreeSpecLike
import smathla.algebra.definitions.{Equal, Higher, Lower}

import scala.util.Random

class IntegerTest extends FreeSpecLike {

  val gen = new Random()

  "Integer" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Integer(a)+Integer(b) == Integer(a+b), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Integer(a)*Integer(b) == Integer(a*b), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Integer(a)-Integer(b) == Integer(a-b), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Integer(a)/Integer(b) == Integer(a/b), s"Error with: $a and $b")
      }
    }
    "method: %" in {
      for(i <- 0 to 10){
        val a = gen.nextInt()
        val b = gen.nextInt()
        assert(Integer(a)%Integer(b) == Integer(a%b), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      assert(!Integer(-1).isPositive, "-1")
      assert(!Integer(0).isPositive, "0")
      assert(Integer(1).isPositive, "1")
    }
    "method: isNegative" in {
      assert(Integer(-1).isNegative, "-1")
      assert(!Integer(0).isNegative, "0")
      assert(!Integer(1).isNegative, "1")
    }
    "method: compareTo" in {
      assert(Integer(-1).compareTo(Integer(1)) === Lower)
      assert(Integer(1).compareTo(Integer(-1)) === Higher)
      assert(Integer(1).compareTo(Integer(1)) === Equal)
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = gen.nextInt()
        assert(-Integer(a) == Integer(-a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!Integer(1).isZero)
      assert(!Integer(-1).isZero)
      assert(Integer(0).isZero)
    }
    "method: isUnit" in {
      assert(Integer(1).isUnit)
      assert(!Integer(0).isUnit)
      assert(!Integer(-1).isUnit)
    }
  }
}
