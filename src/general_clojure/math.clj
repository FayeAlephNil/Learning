(ns general-clojure.math)

(defn natural
  []
  (iterate inc 1))

(defn of-natural
  [func]
  (map func (natural)))

(defn multiples
  [num]
  (of-natural #(* % num)))

(defn divided-by
  [num]
  (of-natural #(/ % num)))

(defn under
  [num]
  (of-natural #(/ num %)))

(def negatives (multiples -1))
(def whole (of-natural #(- % 1)))
