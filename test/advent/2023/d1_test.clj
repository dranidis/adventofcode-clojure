(ns advent.2023.d1-test
  (:require [advent.2023.d1.core :refer [example-1 example-2 input total-2
                                         total-1]]
            [clojure.test :refer [deftest is]]))

(deftest p1
  ;; Answer 1 
  (is (== 142 (total-1 example-1)))
  (is (== 56465 (total-1 input)))
  ;
  )

(deftest p2
  (is (== 281 (total-2 example-2)))
  (is (== 55902 (total-2 input)))
  ;
  )