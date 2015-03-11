(ns general-clojure.algebra
  (:require [general-clojure.util :as u]))

(defprotocol IEval
  "The protocol for evaluation with variables"
  (evaluate [this values] "Evaluates the expression")
  (printexpr [this] "Prints and returns the expresssion"))

(defrecord Expr [expr variables]
  IEval
  (evaluate
    [this values]
    (eval (map #(if (contains? values %) (get values %) %) (:expr this))))
  (printexpr [this] (do (println (:expr this)) (:expr this))))


(defn simplify
  "Simplifies an expression"
  [expr]
  expr
  ;; TODO
  )