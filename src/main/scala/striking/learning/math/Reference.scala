package striking.learning.math

object Reference {
	def whole: Stream[Int] = {
		def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
		loop(0)
	}

	def negative: Stream[Int] = {
		def loop(v: Int): Stream[Int] = v #:: loop(v - 1)
		loop(0)
	}
}
