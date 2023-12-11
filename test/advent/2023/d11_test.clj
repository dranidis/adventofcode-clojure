(ns advent.2023.d11-test
  (:require [advent.2023.d11.core :refer [answer-1 answer-2 example input]]
            [clojure.test :refer [deftest is]]))

(deftest p1
  (is (= 9329143 (answer-1 input)))
  ;
  )

(deftest p2
  (is (= 374 (answer-2 example 2)))
  (is (= 9329143 (answer-2 input 2)))
  (is (= 1030 (answer-2 example 10)))
  (is (= 710674907809 (answer-2 input 1000000)))
  ;
  )