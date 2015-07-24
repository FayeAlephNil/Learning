package striking.learning.math

import striking.learning.Implicits._

import scala.annotation.tailrec
import scala.collection.LinearSeq

object MathImps {
	implicit class SeqInt(xs: Seq[Int]) {
		def unDigits: Int = {
			xs.reduceLeft((a, b) => {
				(a * 10) + b
			})
		}
	}

	implicit class ListIntTuple(xss: List[(Int, Int)]) {
		def sumSame(yss: List[(Int, Int)]): List[(Int, Int)] = {
			if (xss.isEmpty) {
				yss
			} else if (yss.isEmpty) {
				xss
			} else {
				val x = xss.head
				val xs = xss.tail
				val y = yss.head
				val ys = yss.tail

				if (x._1 == y._1) {
					(x._1, x._2 + y._2) :: xs.sumSame(ys)
				} else {
					(x._1, x._2) :: xs.sumSame(yss)
				}
			}
		}
	}

	implicit class IntGeneral(x: Int) {
		def neg: Int = {
			if (x < 0) {
				-1
			} else {
				1
			}
		}

		def factorial: Int = {
			(1 to x).product
		}

		def even: Boolean = {
			x % 2 == 0
		}
	}

	implicit class IntDigits(x: Int) {
		def hash(y: Int): Int = {
			((x-1) % y) + 1
		}

		def digits: List[Int] = {
			@tailrec
			def digitsTail(a: Int, result: List[Int]): List[Int] = {
				val rest: Int = a / 10
				val lastDigit: Int = a % 10
				a match {
					case 0 => result
					case _ => digitsTail(rest, lastDigit :: result)
				}
			}
			digitsTail(x, List[Int]())
		}


		def sumDigits: Int = {
			x.digits.sum
		}

		def digitSum: Int = {
			x.neg * x hash 9
		}

		def minDigitSumList: List[Int] = {
			def loop: Stream[Int] = (x.neg * 9) #:: loop
			x.digitSum :: loop.take(Math.abs(x/9)).toList
		}

		def minDigitSum: Int = {
			x.minDigitSumList.unDigits
		}
	}

	implicit class IntFactors(x: Int) {
		def factorof(y: Int): Boolean = {
			y % x == 0
		}

		def factors: List[Int] = {
			val limit: Int = Math.sqrt(Math.abs(x)).toInt

			val below = (1 to limit).filter { divisor =>
				divisor factorof x
			}

			val upper = below.map { n =>
				x / Math.abs(n)
			}

			(below ++ upper.reverse).distinct.toList
		}

		def prime: Boolean = {
			x.factors == 1 :: Math.abs(x) :: Nil
		}

		def primeFactors: List[Int] = {
			x.factors.filter(_.prime)
		}

		def factorPairs: List[(Int, Int)] = {
			x.factors.combined.toList
		}

		def factorization: List[(Int, Int)] = {
			if (x.prime) {
				(x, 1) :: List[(Int, Int)]()
			} else {
				val factorsX = x.factors
				val factorsXNeeded: List[Int] = factorsX.apply(1) :: factorsX.apply(factorsX.length - 2) :: List[Int]()

				val factorizations = factorsXNeeded.map(_.factorization)

				factorizations.head.sumSame(factorizations.last).toList
			}
		}
	}

	implicit class IntDivisibles(x: Int) {
		def divisibles: LinearSeq[Int] = {
			MathRef.natural.map(_ * x)
		}
	}

	implicit class FloatChecks(x: Float) {
		def isInt: Boolean = x == x.toInt
	}
}
