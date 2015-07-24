package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.FibNumbers.list.take(10).foreach(println)
	println()
	MathRef.FibNumbers.pisano(7).list.take(10).foreach(println)
	println()
	2.divisibles.take(10).foreach(println)
}
