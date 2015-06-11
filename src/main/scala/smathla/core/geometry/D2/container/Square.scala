/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package smathla.core.geometry.D2.container

import Math.Geometry.Container.Container
import Math.Geometry.Point2D
import smathla.calculus
import smathla.core.algebra.structures.impl.real.RealLike

class Square[R <: RealLike[R]](corner: Point2D[R], oppositeCorner: Point2D[R]) extends Container[Point2D[R]] {
  def contains(point: Point2D[R]) =
    calculus.min(corner.x, oppositeCorner.x) <= point.x && point.x <= calculus.max(corner.x, oppositeCorner.x) &&
      calculus.min(corner.y, oppositeCorner.y) <= point.y && point.y <= calculus.max(corner.y, oppositeCorner.y)
}
