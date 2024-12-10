(ns advent.2024.d10.core
  (:require
   [advent.util :refer [str->2D-num]]))

(comment
  (def input
    "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")
  ;
  )

(def input (slurp "src/advent/2024/d10/input.txt"))

(def grid (str->2D-num input))
(def rows (count grid))
(def cols (count (first grid)))

(def starts (for [r (range rows)
                  c (range cols)
                  :when (zero? (get-in grid [r c]))]
              [r c]))

(defn next-positions
  "All possible next positions that are adjacent and 
   their height is one more than the current position.
   Returns an empty list if there are no such positions."
  [[r c]]
  (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [nr (+ r dr)
              nc (+ c dc)]
        :when (some? (get-in grid [nr nc]))
        :when (= (inc (get-in grid [r c]))
                 (get-in grid [nr nc]))]
    [nr nc]))

(defn trail-nexts
  "Given a list of trails, return a list of trails that are one step further.
   It will return an empty list if there is no possible next step for any trail."
  [trails]
  (for [trail trails
        next (next-positions (last trail))]
    (conj trail next)))

(defn all-trails-from
  "Given a starting point, return all possible trails."
  [start]
  (loop [trails [[start]]]
    (let [next-trails (trail-nexts trails)]
      (if (empty? next-trails)
        trails
        (recur next-trails)))))

(def all-trailheads
  (map all-trails-from starts))

(defn score
  "Given a list of trails (trailhead), return the score.
   The number of distint ends."
  [trailhead]
  (count (set (map last trailhead))))

(def answer-1 (apply + (map score all-trailheads)))

;; PART 2
(defn score-2
  "Given a list of trails (trailhead), return the score.
   The number of distinct paths."
  [trailhead]
  (count trailhead))


(def answer-2 (apply + (map score-2 all-trailheads)))

(defn- -main [& _]
  (println "Day 9, Part 1:" answer-1)
  (println "Day 9, Part 2:" answer-2))

(-main)