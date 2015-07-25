package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	def testFunc(x: Int): List[Int] = {
		if (x.intValue().even) {
			x :: 1 :: Nil
		} else {
			List[Int]()
		}
	}

	MathRef.SternSeries.map(testFunc).sterns.take(10).foreach(println)
	println()
	MathRef.SternSeries.flatMap(testFunc).sterns.take(8).foreach(println)
}
