package striking.learning

import striking.learning.math.MathRef
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.BasicLogistic.flatMap(x => x :: x * 2 :: Nil).list.take(10).foreach(println)
}
