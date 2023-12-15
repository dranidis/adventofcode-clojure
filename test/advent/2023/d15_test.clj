(ns advent.2023.d15-test
  (:require [advent.2023.d15.core :refer [answer-1-sum-alg answer-2
                                          hash-algorithm input]]
            [clojure.test :refer [deftest is]]))

(deftest tests
  (is (= 52 (hash-algorithm "HASH")))

  (is (= 1320 (answer-1-sum-alg input)))

  (is (= 507769 (answer-1-sum-alg (slurp "src/advent/2023/d15/input.txt"))))

  (is (= 145 (answer-2 input)))

  (is (= 269747 (answer-2 (slurp "src/advent/2023/d15/input.txt")))))
