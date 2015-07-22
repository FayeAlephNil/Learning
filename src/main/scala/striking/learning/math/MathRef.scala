package striking.learning.math

object MathRef {
	def whole: Stream[Int] = {
		def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
		loop(0)
	}

	def negative: Stream[Int] = {
		def loop(v: Int): Stream[Int] = v #:: loop(v - 1)
		loop(0)
	}

	def Lucas = Fib(2, 1)
	def FibNumbers = Fib(0, 1)
}
