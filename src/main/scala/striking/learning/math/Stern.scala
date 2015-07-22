package striking.learning.math

import MathImps._

import scala.annotation.tailrec
import scala.collection.LinearSeq

object Stern {
	@tailrec
	private def _stern(acc: Int, x: Int): Int = {
		x match {
			case 0 => acc
			case 1 => _stern(acc + 1, 0)
			case _ =>
				if (x.even) {
					_stern(acc, x / 2)
				}
				else {
					val y = x - 1
					val newAcc = acc + stern(y / 2 + 1)
					_stern(newAcc, y / 2)
				}
		}
	}

	def stern(x: Int): Int = {
		sterns.apply(x)
	}

	val sterns: LinearSeq[Int] = MathRef.whole.map(_stern(0, _))
}
