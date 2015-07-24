package striking.learning

import striking.learning.math.MathRef

object Learning extends App {
	MathRef.FibNumbers.list.take(10).foreach(println)
	println()
	MathRef.FibNumbers.pisano(7).list.take(10).foreach(println)
}
