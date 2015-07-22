package striking.learning

import striking.learning.math.MathImps._
import Implicits._
import striking.learning.math.Stern

object Learning extends App {
	(1 :: 2 :: 3 :: List[Int]()).zipWith[Int, Int]((x, y) => x + y, 4 :: 5 :: 6 :: List[Int]()).foreach(println)
	412.factorization.foreach(println)
	println(5.factorial)
	println(0.factorial)
	Stern.sterns.take(10).foreach(println)
}
