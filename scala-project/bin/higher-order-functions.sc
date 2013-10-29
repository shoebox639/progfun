import math.abs

object worksheet {
  def sum(f: Int => Int, a: Int, b: Int) = {
  	def loop(a:Int, acc: Int): Int =
  		if (a > b) acc
  		else loop(a + 1, f(a) + acc)
  	
  	loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  
  sum(x => x, 1, 10)                              //> res0: Int = 55
  
  def prod(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) return 1
  	else f(a) * prod(f)(a + 1, b)             //> prod: (f: Int => Int)(a: Int, b: Int)Int
  
  prod(x => x)(1, 4)                              //> res1: Int = 24
  
  def fact(a: Int): Int =
  prod(x => x)(1, a)                              //> fact: (a: Int)Int
  
  fact(4)                                         //> res2: Int = 24
  
  def combine(c: (Int, Int) => Int, unit: Int)(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) unit
  	else c(f(a), combine(c, unit)(f)(a + 1, b))
                                                  //> combine: (c: (Int, Int) => Int, unit: Int)(f: Int => Int)(a: Int, b: Int)Int
                                                  //| 
  	
	combine((x, y) => x + y, 0) (x => x) (1, 10)
                                                  //> res3: Int = 55
                                       
 	val tolerance = 0.000001                  //> tolerance  : Double = 1.0E-6
  def isCloseEnough(x: Double, y: Double) =
  		abs((x - y) / x) / x < tolerance  //> isCloseEnough: (x: Double, y: Double)Boolean
  		
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess: Double): Double = {
  		val next= f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  def averageDamp(f: Double => Double)(x: Double) =
  	(x + f(x)) / 2                            //> averageDamp: (f: Double => Double)(x: Double)Double
  
  def sqrt(x: Double) =
  	fixedPoint(averageDamp(y => x / y))(x)    //> sqrt: (x: Double)Double
  	
  sqrt(2)                                         //> res4: Double = 1.414213562373095
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}