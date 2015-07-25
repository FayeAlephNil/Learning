package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.FibNumbers.pisano(7).negaList.take(32).foreach(println)
}
