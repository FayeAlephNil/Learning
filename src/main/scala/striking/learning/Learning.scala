package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.FibNumbers.negaList.take(10).foreach(println)
	println()
	println(MathRef.FibNumbers.indexOf(8))
	println()
	2.divisibles.take(10).foreach(println)
}
