(ns advent.2023.d17-new-test
  (:require [advent.2023.d17.core-new :refer [answer1 input next-positions next-positions-2]]
            [clojure.test :refer [deftest is]]))



(deftest day-17-dijkstra
  (is (= 102 (answer1 input next-positions)))
  (is (= 94 (answer1 input next-positions-2)))


  ;
  )