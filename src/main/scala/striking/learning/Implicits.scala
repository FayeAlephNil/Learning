package striking.learning

object Implicits {
	implicit class SeqImplicit[A](xs: Seq[A]) {
		def andNega(negaList: Seq[A]): (Int => A) = {
			(x: Int) => {
				if (x < 0) {
					negaList.apply(x)
				} else {
					xs.apply(x)
				}
			}
		}
	}
}
