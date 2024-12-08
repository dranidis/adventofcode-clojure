(ns advent.2024.d8.core
  (:require
   [advent.util :refer [in-grid? str->2D]]))

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
  "Find the locations of a given antenna in the grid."
  [antenna]
  (for [r (range rows)
        c (range cols)
        :when (= (get-in grid [r c]) antenna)]
    [r c]))

(defn pair-antinodes
  "Generate antinodes between two antenna locations."
  [from-times to-times]
  (fn [[r1 c1] [r2 c2]]
    (let [dr (- r2 r1)
          dc (- c2 c1)]
      (for [n (range from-times to-times)
            antinode [[(+ r2 (* n dr)) (+ c2 (* n dc))]
                      [(- r1 (* n dr)) (- c1 (* n dc))]]
            :when ((in-grid? rows cols) antinode)]
        antinode))))


(defn antinodes
  "Find all antinodes for a given antenna."
  [from-times to-times antenna]
  (for [a1 (antenna-locations antenna)
        a2 (antenna-locations antenna)
        :when (not= a1 a2)
        antinode ((pair-antinodes from-times to-times) a1 a2)]
    antinode))

(def answer-1 (count (set (for [antenna antenna-set
                                antinode (antinodes 1 2 antenna)]
                            antinode))))

;; the answer assumes the worst case scenario where
;; two antennas are at adjacent locations at the edge of the grid
;; and sets to-times to rows
(def answer-2 (count (set (for [antenna antenna-set
                                antinode (antinodes 0 rows antenna)]
                            antinode))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))
