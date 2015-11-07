package smathla.core.algebra.definitions

trait Order
trait TotalOrder

case object Lower extends Order with TotalOrder

case object Higher extends Order with TotalOrder

case object Equal extends Order with TotalOrder

case object NotComparable extends Order


trait TotallyOrderable[A] {

  def compareTo(another: A): TotalOrder

  def <(another: A): Boolean = this.compareTo(another) == Lower

  def >(another: A): Boolean = this.compareTo(another) == Higher

  def ===(another: A): Boolean = this.compareTo(another) == Equal && this.equals(another)

  def <=(another: A) = this < another || this === another

  def >=(another: A) = this > another || this === another

  def !==(another: A) = !(this === another)
}

trait PartiallyOrderable[A] {

  /**
   * precondition: isComparable(another) == true
   */
  def compareTo(another: A): Order

  def isComparableWith(another: A) = this.compareTo(another) == NotComparable


  /**
   * NOTE: If not comparable then false is returned
   */

  def <(another: A): Boolean =  this.compareTo(another) == Lower

  def >(another: A): Boolean = this.compareTo(another) == Higher

  def ===(another: A): Boolean = this.compareTo(another) == Equal && this.equals(another)

  def <=(another: A) = this < another || this === another

  def >=(another: A) = this > another || this === another

  def !==(another: A) = !(this === another)
}