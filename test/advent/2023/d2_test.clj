(ns advent.2023.d2-test
  (:require [advent.2023.d2.core :refer [game-max-colors game-max-of-color
                                         parse-game parse-set sum-of-possible-games
                                         total-power example input]]
            [clojure.test :refer [deftest is]]))

(deftest p1
  (is (= {:blue 3
          :red 4} (parse-set " 3 blue, 4 red")))

  (is (= {:id 1
          :sets [{:blue 3
                  :red 4}
                 {:red 1
                  :green 2
                  :blue 6}
                 {:green 2}]}
         (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))

  (is (= 6 (game-max-of-color {:id 1
                               :sets [{:blue 3
                                       :red 4}
                                      {:red 1
                                       :green 2
                                       :blue 6}
                                      {:green 2}]}
                              :blue)))

  (is (= 8 (sum-of-possible-games example)))

  (is (= 2771 (sum-of-possible-games input))))

;;  part 2
(deftest p2
  (is (= {:red 4 :green 2 :blue 6}
         (game-max-colors {:id 1
                           :sets [{:blue 3
                                   :red 4}
                                  {:red 1
                                   :green 2
                                   :blue 6}
                                  {:green 2}]})))

  (is (= 2286 (total-power example)))

;; answer 2
  (is (= 70924 (total-power input))))