package striking.learning

import striking.learning.math.{Cobweb, MathRef}
import striking.learning.math.MathImps._

object Learning extends App {
	MathRef.BasicLogistic.cobweb.map(t => (t._1 * 2, t._2 * 2)).list.take(10).foreach(println)
}
