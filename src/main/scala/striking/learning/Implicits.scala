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
	}

	implicit class SeqIntImplicit(xs: Seq[Int]) {
		def unDigits: Int = {
			xs.reduceLeft((a, b) => {
				(a * 10) + b
			})
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

		def digitsRev: List[Int] = {
			val rest: Int = x / 10
			val lastDigit: Int = x % 10
			x match {
				case 0 => List[Int]()
				case _ => lastDigit :: rest.digitsRev
			}
		}


		def digits: List[Int] = {
			val digitsRev = x.digitsRev
			digitsRev.reverse
		}


		def sumDigits: Int = {
			x.digitsRev.sum
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
}
