/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Math.Geometry.Container

import Math.Geometry.Point2D

class Cut(containers: Container*) extends Container {
  def contains(point: Point2D) = containers.forall(container => container.contains(point))
}
