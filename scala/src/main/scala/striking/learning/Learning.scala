package striking.learning

import striking.learning.math.{MathImps, LogisticMap}

object Learning extends App {
	new LogisticMap(4, 0.2).cobweb.list.take(10).foreach(println)
	println()
	new LogisticMap(4, 0.2).cobweb.map(_ * 2).list.take(10).foreach(println)
}
