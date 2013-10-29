package week3

object IntSet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(70); 
  val s = Empty.incl(3).incl(4).incl(2);System.out.println("""s  : week3.IntSet = """ + $show(s ));$skip(40); 
  val t = Empty.incl(1).incl(5).incl(3);System.out.println("""t  : week3.IntSet = """ + $show(t ));$skip(15); val res$0 = 
  
  s union t;System.out.println("""res0: week3.IntSet = """ + $show(res$0))}
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
