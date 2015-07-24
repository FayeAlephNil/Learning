package striking.learning.math

object MathRef {
	def numStreamBig(start: BigInt, second: BigInt, f: (BigInt, BigInt) => BigInt): Stream[BigInt] = {
		def loop(v: BigInt, last: BigInt): Stream[BigInt] = {
			v #:: loop(f(v, last), v)
		}
		start #:: loop(second, start)
	}

	def numStream(start: Int, second: Int, f: (Int, Int) => Int): Stream[Int] = {
		def loop(v: Int, last: Int): Stream[Int] = {
			v #:: loop(f(v, last), v)
		}
		start #:: loop(second, start)
	}

	def natural: Stream[Int] = numStream(1, 2, (x: Int, y: Int) => x + 1)

	def whole: Stream[Int] = numStream(0, 1, (x: Int, y: Int) => x + 1)

	def negative0: Stream[Int] = numStream(-1, -2, (x: Int, y: Int) => x - 1)

	def negative: Stream[Int] = numStream(0, -1, (x: Int, y: Int) => x - 1)

	def Lucas = new Fib(2, 1)
	def FibNumbers = new Fib(0, 1)
}
