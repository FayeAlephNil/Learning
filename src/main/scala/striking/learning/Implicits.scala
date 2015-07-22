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
	}
}
