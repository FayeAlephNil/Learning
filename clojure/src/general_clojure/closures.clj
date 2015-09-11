(ns general-clojure.closures)

(defn make-counter
  "Makes a counter with the initial value"
  [initial]
  (let [c (atom initial)] #(swap! c inc)))