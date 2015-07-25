package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.FibNumbers.flatMap { x =>
		if (x.intValue().even) {
			x.toString() :: "1" :: Nil
		} else {
			Nil
		}
	}.negaList.take(10).foreach(println)
}
