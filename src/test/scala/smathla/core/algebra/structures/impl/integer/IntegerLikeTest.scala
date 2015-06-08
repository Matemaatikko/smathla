package smathla.core.algebra.structures.impl.integer

import org.scalatest.FreeSpecLike

class IntegerLikeTest extends FreeSpecLike{

  "IntegerLike - object" - {
    "method: zero" -{
      "for Integer" in {
        assert(Integer(0) == IntegerLike.zero[Integer])
      }
      "for Integer64" in {
        assert(Integer64(0) == IntegerLike.zero[Integer64])
      }
      "for IntegerInf" in {
        assert(IntegerInf(0) == IntegerLike.zero[IntegerInf])
      }
    }
    "method: unit" -{
      "for Integer" in {
        assert(Integer(1) == IntegerLike.unit[Integer])
      }
      "for Integer64" in {
        assert(Integer64(1) == IntegerLike.unit[Integer64])
      }
      "for IntegerInf" in {
        assert(IntegerInf(1) == IntegerLike.unit[IntegerInf])
      }
    }
  }
}
