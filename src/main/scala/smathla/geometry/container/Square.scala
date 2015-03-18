/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Math.Geometry.Container

import Math.Geometry.Point2D

class Square(corner: Point2D, oppositeCorner: Point2D) extends Container {
  def contains(point: Point2D) =
    math.min(corner.getX, oppositeCorner.getX) <= point.getX && point.getX <= math.max(corner.getX, oppositeCorner.getX) &&
      math.min(corner.getY, oppositeCorner.getY) <= point.getY && point.getY <= math.max(corner.getY, oppositeCorner.getY)
}
