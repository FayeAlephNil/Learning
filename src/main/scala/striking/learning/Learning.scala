package striking.learning

import striking.learning.math.MathImps._
import Implicits._
import striking.learning.math.Stern

object Learning extends App {
	val xs: List[Int] = 1 :: 2 :: 3 :: List[Int]()
	val ys: List[Int] = 4 :: 5 :: 6 :: List[Int]()
	xs.zipWith((x: Int, y: Int) => x + y, ys).foreach(println)
	412.factorization.foreach(println)
	println(5.factorial)
	println(0.factorial)
	Stern.sterns.take(10).foreach(println)
}
