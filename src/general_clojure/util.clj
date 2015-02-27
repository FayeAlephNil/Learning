(ns general-clojure.util)

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