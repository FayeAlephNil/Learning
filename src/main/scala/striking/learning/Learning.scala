package striking.learning

import striking.learning.math.{Fib, MathRef}
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.FibNumbers.negaList.take(10).foreach(println)
	println()
	val factor3Fibs: Fib = MathRef.FibNumbers.filter((x: BigInt) => 3 factorof x.intValue())
	factor3Fibs.list.take(10).foreach(println)
	println()
	2.divisibles.take(10).foreach(println)
}
