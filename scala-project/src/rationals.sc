object rationals {
	val x = new Rational(1, 3)                //> x  : Rational = 1/3
	val y = new Rational(5, 7)                //> y  : Rational = 5/7
	val z = new Rational(3, 2)                //> z  : Rational = 3/2
	
	x + y                                     //> res0: Rational = 22/21
	-x                                        //> res1: Rational = 1/-3
	y -x                                      //> res2: Rational = 8/21
	
	x - y - z                                 //> res3: Rational = -79/42
	
	
	
	
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