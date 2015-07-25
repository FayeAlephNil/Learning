package striking.learning

import striking.learning.math.{LogisticMap, Cobweb, MathRef}
import striking.learning.math.MathImps._

object Learning extends App {
	new LogisticMap(4, 0.2).cobweb.map(t => (t._1 * 2, t._2 * 2)).list.take(10).foreach(println)
}
