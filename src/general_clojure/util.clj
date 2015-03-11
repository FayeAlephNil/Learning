(ns general-clojure.util)

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
