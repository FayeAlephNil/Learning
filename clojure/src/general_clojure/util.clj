(ns general-clojure.util)

(defn get-rec
  [lst1 x]
  "Gets the item in index x recursively"
  (if (< x 0)
    (throw (new IndexOutOfBoundsException))
    (loop [lst lst1
      y x]
      (if (= y 0)
        (first lst)
        (recur (rest lst) (- y 1))))))

(defn list-and-nega
  [lst neg-lst]
  "Returns a function that gets from the first list for positive indices and the second for negative indices"
  (memoize (fn [x]
    (if (< x 0)
      (get-rec neg-lst (* x -1))
      (get-rec lst x)))))

(defn in?
  "true if seq contains elm"
  [lst elm]
  (some #(= elm %) lst))

(defn common
  "Finds the things in common between lists"
  ([lst1 lst2]
   (loop [result '() combined (sort (concat lst1 lst2))]
     (if (empty? combined)
       (sort result)
       (if (= (first combined) (second combined))
         (recur (cons (first combined) result) (rest (rest combined)))
         (recur result (rest combined))))))

  ([lst1 lst2 & lsts]
   (->> (common lst1 lst2) (common (reduce common lsts)))))

(defn- curry
  "Helper Func"
  [[params1 params2] body]
  (cons (vec params1)
        (if (empty? params2)
          body
          (list (apply list 'fn (vec params2) body)))))

(defn do-curried
  "Helper Func"
  [symbol1 to-fn params]
  (let [result (split-with (complement vector?) params)
        [[name doc meta] [args & body]] result
        [doc meta] (if (string? doc) [doc meta] [nil doc])
        body (if meta (cons meta body) body)
        arity-for-n #(-> % inc (split-at args) (to-fn body))
        arities (->>
                  (range 0 (count args))
                  (map arity-for-n)
                  reverse)
        before (keep identity [symbol1 name doc])]
    (concat before arities)))

(defmacro defn-curried
  "Builds a multiple arity function similar that returns closures
          for the missing parameters, similar to ML's behaviour."
  [& params]
  (do-curried 'defn curry params))

(defmacro fn-curried
  "Builds a multiple arity function similar that returns closures
          for the missing parameters, similar to ML's behaviour."
  [& params]
  (do-curried 'fn curry params))
