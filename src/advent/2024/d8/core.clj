(ns advent.2024.d8.core
  (:require
   [advent.util :refer [str->2D]]))

(comment
  (def input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")
 ;
  )

(def input (slurp "src/advent/2024/d8/input.txt"))

(def grid (str->2D input))
(def rows (count grid))
(def cols (count (first grid)))

(def antenna-set (set (remove #(= % ".") (flatten grid))))

(defn antenna-locations
  [antenna]
  (for [r (range rows)
        c (range cols)
        :when (= (get-in grid [r c]) antenna)]
    [r c]))

(defn pair-antinodes
  [from-times to-times]
  (fn [[r1 c1] [r2 c2]]
    (let [dr (- r2 r1)
          dc (- c2 c1)]
      (filter (fn [[r c]]
                (and (>= r 0) (>= c 0) (< r rows) (< c cols)))
              (apply concat
                     (for [n (range from-times to-times)]
                       [[(+ r2 (* n dr)) (+ c2 (* n dc))] [(- r1 (* n dr)) (- c1 (* n dc))]]))))))

(defn antinodes
  [from-times to-times antenna]
  (apply concat (for [a1 (antenna-locations antenna)
                      a2 (antenna-locations antenna)
                      :when (not= a1 a2)]
                  ((pair-antinodes from-times to-times) a1 a2))))

(def answer-1 (count (set (apply concat (for [antenna antenna-set]
                                          (antinodes 1 2 antenna))))))

;; the answer assumes the worst case scenario where
;; two antennas are at adjacent locations at the edge of the grid
(def answer-2 (count (set (apply concat (for [antenna antenna-set]
                                          (antinodes 0 rows antenna))))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))
