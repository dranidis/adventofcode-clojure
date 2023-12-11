(ns advent.2023.d10-test
  (:require [advent.2023.d10.core :refer [answer-1 answer-2 parse path]]
            [clojure.test :refer [deftest is]]))


(def example "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def example-8 "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(def example-part2-1 "..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........")

(def example-part2-2 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def example-part2-3 "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(deftest p1

;; (is (= 6778 (answer-1 input)))
  (is (= 4 (answer-1 example)))
  (is (= 8 (answer-1 example-8)))
  ;
  )

(deftest p2

  (is (= 1 (answer-2 (path (parse example-8)))))

  (is (= 1 (answer-2 (path (parse example)))))

  (is (= 4 (answer-2 (path (parse example-part2-1)))))
;; (is (= 8 (answer-2 (path (parse example-part2-2)))))
  (is (= 10 (answer-2 (path (parse example-part2-3)))))
  ;
  )

