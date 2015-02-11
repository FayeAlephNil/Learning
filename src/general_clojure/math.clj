(ns general-clojure.math)

(def natural
  "All natural numbers"
  (iterate inc 1))

(defn of-natural
  "Maps on the natural numbers"
  [func]
  (map func natural))

(defn multiples
  "Multiplies each natural number by the number provided"
  [n]
  (of-natural #(* % n)))

(defn under
  "Maps the natural numbers in the following way number/natural"
  [n]
  (of-natural #(/ % n)))

(defn divided-by
  "Divides each natural number by the number passed in"
  [n]
  (of-natural #(/ n %)))

(defn factorof?
  "Returns whether or not possible is a factor of the number passed in"
  [possible n]
  (zero? (mod n possible)))

(defn factors
  "Returns the factors of given number"
  [n]
  (let [factors-below-sqrt (filter #(factorof? % n) (range 1 (inc (Math/sqrt n))))
        factors-above-sqrt (map #(/ n %) factors-below-sqrt)]
    (sort (distinct (concat factors-below-sqrt factors-above-sqrt)))))

(def factors
  "Returns the factors of given number"
  (memoize factors))

(defn prime?
  "Checks if a number is prime"
  [n]
  (= (factors n) [1 n]))

(defn prime-factors
  "Gives the prime-factors of a number"
  [n]
  (filter prime? (factors n)))


(defn factor-pairs
  "Gives the factor pairs for a number"
  [n]
  (loop [the-factors (factors n) result ()]
    (if (empty? the-factors)
      result
      (recur (rest (butlast the-factors)) (cons [(first the-factors) (last the-factors)] result)))))

(def factor-pairs
  "Gives the factor pairs for a number"
  (memoize factor-pairs))

(defn prime-factorization
  "Gives the prime factorization of a number"
  [n]
  (loop [remaining (seq (first (factor-pairs n)))]
    (if (filter #((or (not (prime? %)) (=  % 1))) remaining)
      remaining
      (recur ((prime-factorization (first remaining)) (prime-factorization (last remaining)))))))

(defn sum-factors
  "Sums the factors of a number"
  [n]
  (reduce + (factors n)))

(def sum-factors
  "Sums the factors of a number"
  (memoize sum-factors))

(defn perfect?
  "Tells if a number is perfect"
  [n]
  (= (sum-factors n) (* 2 n)))

(defn abundant?
  "Tells if a number is abundant"
  [n]
  (> (sum-factors n) (* 2 n)))

(defn deficient?
  "Tells if a number is deficient"
  [n]
  (< (sum-factors n) (* 2 n)))

(defn factorial
  "Finds the factorial of a number"
  [n]
  (loop [cnt n acc 1]
    (if (zero? cnt)
      acc
      (recur (dec cnt) (* acc cnt)))))

(def negatives
  "All the negative numbers"
  (multiples -1))
(def whole
  "All the whole numbers"
  (of-natural dec))
(def primes
  "All the primes"
  (filter prime? natural))
(def perfects
  "All the perfect numbers"
  (filter perfect? natural))