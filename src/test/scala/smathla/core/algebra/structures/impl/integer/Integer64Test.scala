package smathla.core.algebra.structures.impl.integer

import org.scalatest.FreeSpecLike
import smathla.core.algebra.definitions.{Equal, Lower, Higher}

import scala.util.Random


class Integer64Test extends FreeSpecLike{
  val gen = new Random()

  "Integer" - {
    "method: +" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Integer64(a)+Integer64(b) == Integer64(a+b), s"Error with: $a and $b")
      }
    }
    "method: *" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Integer64(a)*Integer64(b) == Integer64(a*b), s"Error with: $a and $b")
      }
    }
    "method: -" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Integer64(a)-Integer64(b) == Integer64(a-b), s"Error with: $a and $b")
      }
    }
    "method: /" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Integer64(a)/Integer64(b) == Integer64(a/b), s"Error with: $a and $b")
      }
    }
    "method: %" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        assert(Integer64(a)%Integer64(b) == Integer64(a%b), s"Error with: $a and $b")
      }
    }
    "method: gcd" in {
      for(i <- 0 to 10){
        val a = gen.nextLong()
        val b = gen.nextLong()
        val answer = Integer64(a).gcd(Integer64(b))
        assert((answer | Integer64(a)) && (answer | Integer64(b)), s"Error with: $a and $b")
      }
    }
    "method: isPositive" in {
      assert(!Integer64(-1).isPositive, "-1")
      assert(!Integer64(0).isPositive, "0")
      assert(Integer64(1).isPositive, "1")
    }
    "method: isNegative" in {
      assert(Integer64(-1).isNegative, "-1")
      assert(!Integer64(0).isNegative, "0")
      assert(!Integer64(1).isNegative, "1")
    }
    "method: compareTo" in {
      assert(Integer64(-1).compareTo(Integer64(1)) === Lower)
      assert(Integer64(1).compareTo(Integer64(-1)) === Higher)
      assert(Integer64(1).compareTo(Integer64(1)) === Equal)
    }
    "method: unary_-" in {
      for(a <- 0 to 10){
        val a = gen.nextLong()
        assert(-Integer64(a) == Integer64(-a), s"Error with: $a")
      }
    }
    "method: isZero" in {
      assert(!Integer64(1).isZero)
      assert(!Integer64(-1).isZero)
      assert(Integer64(0).isZero)
    }
    "method: isUnit" in {
      assert(Integer64(1).isUnit)
      assert(!Integer64(0).isUnit)
      assert(!Integer64(-1).isUnit)
    }
  }
}
