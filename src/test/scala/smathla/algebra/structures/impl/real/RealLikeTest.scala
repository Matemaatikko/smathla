package smathla.algebra.structures.impl.real

import org.scalatest.FreeSpecLike


class RealLikeTest extends FreeSpecLike{

  "IntegerLike - object" - {
    "method: zero" -{
      "for Real" in {
        assert(Real(0) == RealLike.zero[Real])
      }
      "for Real64" in {
        assert(Real64(0) == RealLike.zero[Real64])
      }
    }
    "method: unit" -{
      "for Integer" in {
        assert(Real(1) == RealLike.unit[Real])
      }
      "for Real64" in {
        assert(Real64(1) == RealLike.unit[Real64])
      }
    }
  }
}

