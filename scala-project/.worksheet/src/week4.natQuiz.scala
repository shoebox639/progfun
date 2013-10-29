package week4

object natQuiz {
  
	abstract class Nat {
		def isZero: Boolean
		def predecessor: Nat
		def successor:Nat = new Succ(this)
		def + (x: Nat): Nat
		def - (x: Nat): Nat
		
		def toStringAcc(acc: Int): String
			
	}
	
	object Zero extends Nat {
		def isZero:Boolean  = true
		
		def predecessor: Nat = throw new IllegalArgumentException
		def + (x: Nat): Nat = x
		def - (x: Nat): Nat = throw new IllegalArgumentException
		
		override def toString: String = "0"
		def toStringAcc(acc: Int): String = acc.toString
	}
	
	class Succ(n: Nat) extends Nat {
		def isZero: Boolean = false
		def predecessor: Nat = n
		def + (x:Nat): Nat =
			if (x.isZero) new Succ(n)
			else successor + x.predecessor
		def - (x:Nat): Nat =
			if (x.isZero) new Succ(n)
			else n - x.predecessor
	
		override def toString: String = toStringAcc(0)
		def toStringAcc(acc: Int): String =
			if (isZero) acc.toString
			else predecessor.toStringAcc(acc + 1)
	};import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(966); 
	
	val zero = Zero;System.out.println("""zero  : week4.natQuiz.Zero.type = """ + $show(zero ));$skip(28); 
	
	val one = new Succ(Zero);System.out.println("""one  : week4.natQuiz.Succ = """ + $show(one ));$skip(25); 
	val two = new Succ(one);System.out.println("""two  : week4.natQuiz.Succ = """ + $show(two ));$skip(19); val res$0 = 
	
	one.predecessor;System.out.println("""res0: week4.natQuiz.Nat = """ + $show(res$0));$skip(17); val res$1 = 
	two.predecessor;System.out.println("""res1: week4.natQuiz.Nat = """ + $show(res$1));$skip(15); val res$2 = 
	one.successor;System.out.println("""res2: week4.natQuiz.Nat = """ + $show(res$2));$skip(15); val res$3 = 
	two.successor;System.out.println("""res3: week4.natQuiz.Nat = """ + $show(res$3));$skip(14); val res$4 = 
	
	zero + one;System.out.println("""res4: week4.natQuiz.Nat = """ + $show(res$4));$skip(12); val res$5 = 
	zero + two;System.out.println("""res5: week4.natQuiz.Nat = """ + $show(res$5));$skip(25); 
	
	val three = one + two;System.out.println("""three  : week4.natQuiz.Nat = """ + $show(three ));$skip(25); 
	val six = three + three;System.out.println("""six  : week4.natQuiz.Nat = """ + $show(six ));$skip(28); 
	val nine = six + two + one;System.out.println("""nine  : week4.natQuiz.Nat = """ + $show(nine ));$skip(26); 
	
	val seven = nine - two;System.out.println("""seven  : week4.natQuiz.Nat = """ + $show(seven ));$skip(24); 
	val five = seven - two;System.out.println("""five  : week4.natQuiz.Nat = """ + $show(five ))}
	
}
