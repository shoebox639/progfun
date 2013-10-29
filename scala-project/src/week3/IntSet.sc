package week3

object IntSet {
  val s = Empty.incl(3).incl(4).incl(2)           //> s  : week3.IntSet = {{.2.}3{.4.}}
  val t = Empty.incl(1).incl(5).incl(3)           //> t  : week3.IntSet = {.1{{.3.}5.}}
  
  s union t                                       //> res0: week3.IntSet = {.1{{{.2.}3{.4.}}5.}}
}

abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

object Empty extends IntSet {
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other
	override def toString(): String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	val this.elem = elem
	val this.left = left
	val this.right = right
	
	def incl(x: Int): IntSet =
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	def contains(x: Int): Boolean =
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
	override def toString(): String = "{" + left + elem + right + "}"
	def union(other: IntSet): IntSet = {
		((left union right) union other) incl elem
	}
}