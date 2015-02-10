(ns general-clojure.math)

(def factor-sums {})

(defn natural
  []
  (iterate inc 1))

(defn of-natural
  [func]
  (map func (natural)))

(defn multiples
  [num]
  (of-natural #(* % num)))

(defn under
  [num]
  (of-natural #(/ % num)))

(defn divided-by
  [num]
  (of-natural #(/ num %)))

(defn factorof?
  [possible num]
  (= (mod num possible) 0))

(defn factors-
  [num]
  (let [factors-below-sqrt (filter #(factorof? % num) (range 1 (inc (Math/sqrt num))))
        factors-above-sqrt (map #(/ num %) factors-below-sqrt)]
    (distinct (concat factors-below-sqrt factors-above-sqrt))))

(def factors (memoize factors-))

(defn sum-factors-
  [num]
  (reduce + (factors num)))

(def sum-factors (memoize sum-factors-))

(defn perfect?
  [num]
  (= (sum-factors num) (* 2 num)))

(defn abundant?
  [num]
  (> (sum-factors num) (* 2 num)))

(defn deficient?
  [num]
  (< (sum-factors num) (* 2 num)))

(def negatives (multiples -1))
(def whole (of-natural #(- % 1)))
