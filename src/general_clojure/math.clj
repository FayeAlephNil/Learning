(ns general-clojure.math
  (:require [general-clojure.util :as u]))

(def natural
  "All natural numbers"
  (iterate inc 1))

(defn of-natural
  "Maps on the natural numbers"
  [func]
  (pmap func natural))

(defn multiples
  "Multiplies each natural number by the number provided"
  [n]
  (of-natural #(* % n)))

(def negatives
  "All the negative numbers"
  (multiples -1))

(defn divided-by
  "Divides each natural number by the number passed in"
  [n]
  (of-natural #(/ % n)))

(defn under
  "Maps the natural numbers in the following way number/natural"
  [n]
  (of-natural #(/ n %)))

(defn palindrome?
  "Checks whether or not a number is a palindrome"
  [n]
  (== n (->> (str n) reverse (apply str) Integer/parseInt)))

(defn factorof?
  "Returns whether or not possible is a factor of the number passed in"
  [possible n]
  (zero? (mod n possible)))

(defn factors
  "Returns the factors of given number"
  [n]
  (let [n (if (< n 0) (* n -1) n)
        factors-below-sqrt (filter #(factorof? % n) (take (Math/sqrt n) natural))
        factors-above-sqrt (pmap #(/ n %) factors-below-sqrt)]
    (->> (concat factors-below-sqrt factors-above-sqrt) sort distinct)))

(def factors
  "Returns the factors of given number"
  (memoize factors))

(defn gcf
  "Finds the greatest common factor of the numbers"
  [n m & nums]
  (->> (concat (list n m) nums) (map factors) (apply u/common) last))

(def gcf
  "Finds the greatest common factor of the numbers"
  (memoize gcf))

(defn lcm
  "Finds the least common denominator of the nums"
  ([n m]
   (->> (apply gcf [n m]) (/ (reduce * [(if (< n 0) (* n -1) n) (if (< m 0) (* m -1) n)]))))
  ([n m & nums]
   (let [nums (concat (list n m) nums)]
     (reduce lcm nums))))

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
      (recur (-> (butlast the-factors) rest) (cons [(first the-factors) (last the-factors)] result)))))

(def factor-pairs
  "Gives the factor pairs for a number"
  (memoize factor-pairs))

(defn prime-factorization
  "Gets the prime factorization of a number"
  ([n] (if (prime? n) (list 1 n) (prime-factorization n 2)))
  ([n candidate]
   (cond (< n candidate) []
         (zero? (rem n candidate))
         (cons candidate (prime-factorization (/ n candidate) candidate))
         :else (prime-factorization n (+ 1 candidate)))))

(def prime-factorization
  "Gets the prime factorization of a number"
  (memoize prime-factorization))

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

(def whole
  "All the whole numbers"
  (of-natural dec))
(def primes
  "All the primes"
  (filter prime? natural))
(def perfects
  "All the perfect numbers"
  (filter perfect? natural))