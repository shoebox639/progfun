object rationals {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(46); 
	val x = new Rational(1, 3);System.out.println("""x  : Rational = """ + $show(x ));$skip(28); 
	val y = new Rational(5, 7);System.out.println("""y  : Rational = """ + $show(y ));$skip(28); 
	val z = new Rational(3, 2);System.out.println("""z  : Rational = """ + $show(z ));$skip(9); val res$0 = 
	
	x + y;System.out.println("""res0: Rational = """ + $show(res$0));$skip(4); val res$1 = 
	-x;System.out.println("""res1: Rational = """ + $show(res$1));$skip(6); val res$2 = 
	y -x;System.out.println("""res2: Rational = """ + $show(res$2));$skip(13); val res$3 = 
	
	x - y - z;System.out.println("""res3: Rational = """ + $show(res$3))}
	
	
	
	
}

class Rational(x: Int, y: Int) {
	require(y != 0, "denom can't be null")
	
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int):Int = if (b == 0) a else gcd(b, a % b)
	val g = gcd(x, y)
	val numer = x;
	val denom = y;
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	def > (that: Rational) = numer * that.denom > that.numer * denom
		
	def max(that: Rational) = if (this < (that)) that else this
	
	def +(that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom
		)
		
	def unary_- = new Rational(-numer, denom)
		
	def -(that: Rational) = this + -that
		
		 
	override def toString():String = numer / g + "/" + denom /g
}
