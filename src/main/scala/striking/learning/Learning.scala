package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.FibNumbers.negaList.take(10).foreach(println)
	println()
	MathRef.FibNumbers.filter(_.intValue().even).take(10).foreach(println)
	println()
	2.divisibles.take(10).foreach(println)
}
