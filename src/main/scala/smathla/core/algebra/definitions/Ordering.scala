package smathla.core.algebra.definitions

trait Order

case object Lower extends Order

case object Higher extends Order

case object Equal extends Order


trait TotallyOrderable[A] {

  def compareTo(another: A): Order

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

  def isComparableWith(another: A)

  def <(another: A): Boolean = this.compareTo(another) == Lower

  def >(another: A): Boolean = this.compareTo(another) == Higher

  def ===(another: A): Boolean = this.compareTo(another) == Equal && this.equals(another)

  def <=(another: A) = this < another || this === another

  def >=(another: A) = this > another || this === another

  def !==(another: A) = !(this === another)
}