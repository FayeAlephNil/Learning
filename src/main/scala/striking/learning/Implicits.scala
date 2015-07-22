package striking.learning

import scala.annotation.tailrec

object Implicits {
	implicit class SeqImplicit[A](xs: Seq[A]) {
		def andNega(negaList: Seq[A]): (Int => A) = {
			(x: Int) => {
				if (x < 0) {
					negaList.apply(-x)
				} else {
					xs.apply(x)
				}
			}
		}

		def combined: List[(A, A)] = {
			if (xs == Seq[A]()) {
				List[(A, A)]()
			} else if (xs.length == 1) {
				(xs.head, xs.head) :: List[(A, A)]()
			} else {
				(xs.head, xs.last) :: xs.tail.init.combined
			}
		}
	}

	implicit class SeqIntImplicit(xs: Seq[Int]) {
		def unDigits: Int = {
			xs.reduceLeft((a, b) => {
				(a * 10) + b
			})
		}
	}

	implicit class ListIntTupleImplicit(xss: List[(Int, Int)]) {
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

	implicit class IntImplicit(x: Int) {
		def hash(y: Int): Int = {
			((x-1) % y) + 1
		}

		def neg: Int = {
			if (x < 0) {
				-1
			} else {
				1
			}
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
}
