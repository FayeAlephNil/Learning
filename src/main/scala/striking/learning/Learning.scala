package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	val factorizations = MathRef.SternSeries.map(_.factorization).sterns
	MathRef.SternSeries.sterns.zip(factorizations).take(20).foreach(println)
}
