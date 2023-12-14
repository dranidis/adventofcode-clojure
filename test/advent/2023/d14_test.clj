(ns advent.2023.d14-test
  (:require [advent.2023.d14.core :refer [answer-1 answer-2 input tilt-a-rock
                                          tilt-rocks]]
            [clojure.test :refer [deftest is testing]]))

(deftest p1

  (testing "tilt-a-rock"
    (is (= [0 6 7] (tilt-a-rock 2 9 [0 6 9] [5]))))

  (testing "tilt-rocks"
    (is (= [[0 1 2 3] [8 9]] (tilt-rocks [[0 1 2 3] [8 9]])))
    (is (= [[0 6 7] [5]] (tilt-rocks [[1 6 9] [5]])))
    (is (= [[2 6] [1 5]] (tilt-rocks [[3 6] [1 5]]))))

  (is (= 136 (answer-1 input)))

  ;
  )

(deftest part2

  (is (= 64 (answer-2 input 1000000000))))