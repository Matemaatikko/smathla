package smathla.core.geometry

import shapeless.{Sized, Nat}


class Point[N <: Nat, R](val coordinates: Sized[Seq[R], N]){

}
