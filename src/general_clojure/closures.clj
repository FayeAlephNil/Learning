(ns general-clojure.closures)

(defn make-counter
  [initial]
  (let [c (atom initial)] #(swap! c inc)))