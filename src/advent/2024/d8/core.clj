(ns advent.2024.d8.core
  (:require
   [advent.util :refer [coords-of-pred in-grid? str->2D]]))

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
  (coords-of-pred grid #(= % antenna)))

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

;;
;; Can also be solved with a SINGLE
;; for comprehension
;;
(defn alt-answer
  [from-times to-times]
  (count (set (for [antenna (vec antenna-set)
                    r1 (range rows)
                    c1 (range cols)
                    :when (= antenna (get-in grid [r1 c1]))
                    r2 (range rows)
                    c2 (range cols)
                    :when (not= [r1 c1] [r2 c2])
                    :when (= antenna (get-in grid [r2 c2]))
                    n (range from-times to-times)
                    :let [dr (- r2 r1)
                          dc (- c2 c1)]
                    a [[(+ r2 (* n dr)) (+ c2 (* n dc))]
                       [(- r1 (* n dr)) (- c1 (* n dc))]]
                    :when ((in-grid? rows cols) a)]
                a))))

(println "Alt: Day 8, Part 1:" (alt-answer 1 2))
(println "Alt: Day 8, Part 2:" (alt-answer 0 50))





