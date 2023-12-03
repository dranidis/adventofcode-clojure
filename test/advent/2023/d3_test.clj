(ns advent.2023.d3-test
  (:require [advent.2023.d3 :refer [answer1 answer2 coords-of-adjacent-symbols
                                    make-table next-stars-coords star-coords]]
            [clojure.test :refer [deftest is testing]]))

(def input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(deftest p1

  (testing "coords-of-adjacent-symbols"
    (is (= (list [[1 3] \*]) (coords-of-adjacent-symbols (make-table input) 0 2)))
    (is (= (list) (coords-of-adjacent-symbols (make-table input) 0 0)))
    (is (= (list) (coords-of-adjacent-symbols (make-table input) 0 1))))

  (testing "next-stars-coords"
    (is (= (list [1 3]) (next-stars-coords (make-table input) 0 2))))

  (is (= 4361 (answer1 input)))

  ;
  )

(deftest p2
  (is (= (list [1 3] [4 3] [8 5])
         (star-coords (make-table input))))

  (is (= 467835 (answer2 input)))

  ;
  )