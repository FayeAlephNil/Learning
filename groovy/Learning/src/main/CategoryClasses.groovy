package main

import main.math.Fib

class CategoryClasses {
	static class IntegerCategory {
		static BigInteger getFib(Integer self) {
			Fib.fibNums.get(self)
		}

		static BigInteger getLucas(Integer self) {
			Fib.lucasNums.get(self)
		}

		static BigInteger getFactorial(Integer self) {
			getFactorialRec(self)
		}

		private static BigInteger getFactorialRec(Integer self, BigInteger acc = 1) {
			if (self < 0) {
				throw new IllegalArgumentException("Passed negative to getFactorial function: $self")
			} else if (self == 0) {
				acc
			} else {
				getFactorialRec(self - 1, acc * self.toBigInteger())
			}
		}
	}

	static class ListCategory {
		static andNega(self, negaList) {
			{
				int idx -> if (idx < 0) negaList.get(-idx) else self.get(idx)
			}
		}
	}
}
