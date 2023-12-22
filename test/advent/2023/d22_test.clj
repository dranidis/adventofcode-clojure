(ns advent.2023.d22-test
  (:require [advent.2023.d22.core :refer :all]
            [clojure.test :refer [deftest is]]))

(deftest p
  (is (supports? [[1 0 1] [1 2 1]] [[0 0 2] [2 0 2]]))

  (is (= [[1 1 9]] (top-brick-coords [[1 1 8] [1 1 9]])))
  (is (= [[1 1 8]] (bottom-brick-coords [[1 1 8] [1 1 9]])))

  (is (= 5 (count (all-that-can-be-removed (parse input)))))

  (is (= [[1 0 1] [1 2 1]]
         (falls-until-supported []
                                [[1 0 1] [1 2 1]])))
  (is (= [[0 0 2] [2 0 2]]
         (falls-until-supported [[[1 0 1] [1 2 1]]]
                                [[0 0 2] [2 0 2]])))
  (is (= [[0 2 2] [2 2 2]]
         (falls-until-supported [[[1 0 1] [1 2 1]] [[0 0 2] [2 0 2]]]
                                [[0 2 3] [2 2 3]])))
  (is (= [[1 1 5] [1 1 6]]
         (falls-until-supported [[[1 0 1] [1 2 1]] [[0 0 2] [2 0 2]] [[0 2 2] [2 2 2]] [[0 0 3] [0 2 3]] [[2 0 3] [2 2 3]] [[0 1 4] [2 1 4]]]
                                [[1 1 8] [1 1 9]])))

  (is (= 5 (answer-1 input)))
  (is (= 7 (answer-2 input)))
  ;
  )
