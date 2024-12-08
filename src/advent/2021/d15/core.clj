(ns advent.2021.d15.core
  (:require
   [advent.2023.d17.dijkstra :refer [dijkstra-shortest-distances]]
   [advent.util :refer [str->2D-num]]))

(comment
  (def input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
 ;
  )

(def input (slurp "src/advent/2021/d15/input.txt"))

(def grid (str->2D-num input))
(def rows (count grid))
(def cols (count (first grid)))

(def start [0 0])
(def end [(dec rows) (dec cols)])

(defn neighbors
  [[r c]]
  (for [dir [[0 1] [1 0] [0 -1] [-1 0]]
        :let [nr (+ r (first dir))
              nc (+ c (second dir))]
        :when (and (>= nr 0) (< nr rows)
                   (>= nc 0) (< nc cols))]
    [(get-in grid [nr nc]) [nr nc]]))

(def answer-1 ((dijkstra-shortest-distances start neighbors) end))

;; Part 2
(def values (vec (range 1 10)))

(defn cell-content
  [br bc nr nc]
  (let [v (get-in grid [nr nc])]
    (get values (mod (dec (+ v br bc)) 9))))

(defn get-grid-cell
  [r c]
  (let [nr (mod r rows)
        nc (mod c cols)
        br (quot r rows)
        bc (quot c cols)]
    (cell-content br bc nr nc)))

(defn neighbors-5
  [[r c]]
  (for [dir [[0 1] [1 0] [0 -1] [-1 0]]
        :let [nr (+ r (first dir))
              nc (+ c (second dir))]
        :when (and (>= nr 0) (< nr (* 5 rows))
                   (>= nc 0) (< nc (* 5 cols)))]
    [(get-grid-cell nr nc) [nr nc]]))

(def answer-2 ((dijkstra-shortest-distances start neighbors-5)
               [(dec (* 5 rows)) (dec (* 5 cols))]))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))


