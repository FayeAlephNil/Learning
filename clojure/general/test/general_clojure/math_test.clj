(ns general-clojure.math-test
  (:require [clojure.test :refer :all]
            [general-clojure.math :refer :all]))


(deftest test-natural
  (testing "Make sure the natural numbers work"
    (is (= (take 5 natural) (list 1 2 3 4 5)) "Make sure of the natural nums")))

(deftest test-of-natural
  (testing "Make sure of-natural works correctly"
    (is (= (take 5 (of-natural #(- % 1))) (list 0 1 2 3 4)) "Whole numbers, and thus of-natural working")))

