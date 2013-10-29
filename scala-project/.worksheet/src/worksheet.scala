import math.abs

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(190); 
  def sum(f: Int => Int, a: Int, b: Int) = {
  	def loop(a:Int, acc: Int): Int =
  		if (a > b) acc
  		else loop(a + 1, f(a) + acc)
  	
  	loop(a, 0)
  };System.out.println("""sum: (f: Int => Int, a: Int, b: Int)Int""");$skip(24); val res$0 = 
  
  sum(x => x, 1, 10);System.out.println("""res0: Int = """ + $show(res$0));$skip(108); 
  
  def prod(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) return 1
  	else f(a) * prod(f)(a + 1, b);System.out.println("""prod: (f: Int => Int)(a: Int, b: Int)Int""");$skip(24); val res$1 = 
  
  prod(x => x)(1, 4);System.out.println("""res1: Int = """ + $show(res$1));$skip(50); 
  
  def fact(a: Int): Int =
  prod(x => x)(1, a);System.out.println("""fact: (a: Int)Int""");$skip(13); val res$2 = 
  
  fact(4);System.out.println("""res2: Int = """ + $show(res$2));$skip(154); 
  
  def combine(c: (Int, Int) => Int, unit: Int)(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) unit
  	else c(f(a), combine(c, unit)(f)(a + 1, b));System.out.println("""combine: (c: (Int, Int) => Int, unit: Int)(f: Int => Int)(a: Int, b: Int)Int""");$skip(50); val res$3 = 
  	
	combine((x, y) => x + y, 0) (x => x) (1, 10);System.out.println("""res3: Int = """ + $show(res$3));$skip(67); 
                                       
 	val tolerance = 0.000001;System.out.println("""tolerance  : Double = """ + $show(tolerance ));$skip(81); 
  def isCloseEnough(x: Double, y: Double) =
  		abs((x - y) / x) / x < tolerance;System.out.println("""isCloseEnough: (x: Double, y: Double)Boolean""");$skip(228); 
  		
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess: Double): Double = {
  		val next= f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  };System.out.println("""fixedPoint: (f: Double => Double)(firstGuess: Double)Double""");$skip(73); 
  
  def averageDamp(f: Double => Double)(x: Double) =
  	(x + f(x)) / 2;System.out.println("""averageDamp: (f: Double => Double)(x: Double)Double""");$skip(69); 
  
  def sqrt(x: Double) =
  	fixedPoint(averageDamp(y => x / y))(x);System.out.println("""sqrt: (x: Double)Double""");$skip(14); val res$4 = 
  	
  sqrt(2);System.out.println("""res4: Double = """ + $show(res$4))}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
