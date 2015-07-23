package striking.learning

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

		def zipWith[B, C](f: (A, B) => C, ys: Seq[B]): List[C] = {
			if (xs.isEmpty || ys.isEmpty) {
				List[C]()
			} else {
				f(xs.head, ys.head) :: xs.tail.zipWith(f, ys.tail)
			}
		}

		def groupsOf(n: Int): List[Seq[A]] = {
			if (xs.isEmpty) {
				List[Seq[A]]()
			} else {
				xs.take(n) :: xs.drop(n).groupsOf(n)
			}
		}

		def split: List[Seq[A]] = {
			xs.groupsOf(xs.length / 2)
		}
	}
}
