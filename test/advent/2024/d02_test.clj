(ns advent.2024.d02-test
  (:require
   [advent.2024.d2.core :refer [example input answer-1 answer-2]]
   [clojure.test :refer [deftest is]]))

(deftest input-test
  (is (== 314 (answer-1 input)))
  (is (== 373 (answer-2 input))))

(deftest example-test
  (is (== 2 (answer-1 example)))
  (is (== 4 (answer-2 example))))