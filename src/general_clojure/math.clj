(ns general-clojure.math)

(defn natural
  "Returns all natural numbers"
  []
  (iterate inc 1))

(defn of-natural
  "Maps on the natural numbers"
  [func]
  (map func (natural)))

(defn multiples
  "Multiplies each natural number by the number provided"
  [num]
  (of-natural #(* % num)))

(defn under
  "Maps the natural numbers in the following way num/natural"
  [num]
  (of-natural #(/ % num)))

(defn divided-by
  "Divides each natural number by num"
  [num]
  (of-natural #(/ num %)))

(defn factorof?
  "Returns whether or not possible is a factor of num"
  [possible num]
  (zero? (mod num possible)))

(defn factors-
  "Returns the factors of given num"
  [num]
  (let [factors-below-sqrt (filter #(factorof? % num) (range 1 (inc (Math/sqrt num))))
        factors-above-sqrt (map #(/ num %) factors-below-sqrt)]
    (distinct (concat factors-below-sqrt factors-above-sqrt))))

(def factors (memoize factors-))

(defn sum-factors-
  "Sums the factors of a number"
  [num]
  (reduce + (factors num)))

(def sum-factors (memoize sum-factors-))

(defn perfect?
  "Tells if a number is perfect"
  [num]
  (= (sum-factors num) (* 2 num)))

(defn abundant?
  "Tells if a number is abundant"
  [num]
  (> (sum-factors num) (* 2 num)))

(defn deficient?
  "Tells if a number is deficient"
  [num]
  (< (sum-factors num) (* 2 num)))

(def negatives (multiples -1))
(def whole (of-natural #(dec %)))
