package idealized.scala

abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T
  
  def && (x: Boolean): Boolean = ifThenElse(x, False)
  def || (x: Boolean): Boolean = ifThenElse(True, x)
  def unary_!(): Boolean = ifThenElse(False, True)
  
  def == (x: Boolean): Boolean = ifThenElse(x, !x)
  def != (x: Boolean): Boolean = ifThenElse(!x, x)
  
  def < (x: Boolean): Boolean = ifThenElse(False, x)
}

object True extends Boolean {
  override def ifThenElse[T](t: => T, e: => T): T = t
}

object False extends Boolean {
  override def ifThenElse[T](t: => T, e: => T): T = e
}