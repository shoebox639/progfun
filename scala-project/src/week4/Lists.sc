object Lists {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 
  List()                                          //> res0: List[Nothing] = Nil@4538353
  List(1) foreach println                         //> 1
  List(1, 2) foreach println                      //> 1
                                                  //| 2
  
}

trait List[T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
  def foreach(f: T => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}


class Nil[T] extends List[T] {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object List {
	def apply[T]():List[T] = new Nil
	def apply[T](e: T): List[T] = new Cons(e, this())
	def apply[T](e1: T, e2: T): List[T] = new Cons(e1, this(e2))
}