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
	}
	
	val zero = Zero                           //> zero  : week4.natQuiz.Zero.type = 0
	
	val one = new Succ(Zero)                  //> one  : week4.natQuiz.Succ = 1
	val two = new Succ(one)                   //> two  : week4.natQuiz.Succ = 2
	
	one.predecessor                           //> res0: week4.natQuiz.Nat = 0
	two.predecessor                           //> res1: week4.natQuiz.Nat = 1
	one.successor                             //> res2: week4.natQuiz.Nat = 2
	two.successor                             //> res3: week4.natQuiz.Nat = 3
	
	zero + one                                //> res4: week4.natQuiz.Nat = 1
	zero + two                                //> res5: week4.natQuiz.Nat = 2
	
	val three = one + two                     //> three  : week4.natQuiz.Nat = 3
	val six = three + three                   //> six  : week4.natQuiz.Nat = 6
	val nine = six + two + one                //> nine  : week4.natQuiz.Nat = 9
	
	val seven = nine - two                    //> seven  : week4.natQuiz.Nat = 7
	val five = seven - two                    //> five  : week4.natQuiz.Nat = 5
	
}