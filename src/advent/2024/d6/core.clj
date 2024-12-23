(ns advent.2024.d6.core
  (:require
   [advent.util :refer [coords-of-symbol str->2D]]))

(comment
  (def input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")
 ;
  )

(def input (slurp "src/advent/2024/d6/input.txt"))

(def the-map (str->2D input))

(def guard-pos (first (coords-of-symbol the-map "^")))

(def change-dir
  (let [change-dir-fn (fn [dir]
                        (case dir
                          [0 1] [1 0] ;; down
                          [1 0] [0 -1] ;; left
                          [0 -1] [-1 0] ;; up
                          [-1 0] [0 1] ;; right
                          ))]
    (into {} (map (juxt identity change-dir-fn)
                  [[0 1] [1 0] [0 -1] [-1 0]]))))

(def visited
  (loop [visited #{guard-pos}
         pos guard-pos
         dir [-1 0]]
    (let [new-r (+ (first pos) (first dir))
          new-c (+ (second pos) (second dir))
          new-cell (get-in the-map [new-r new-c])]
      (if (nil? new-cell)
        visited
        (if (= new-cell "#")
          (recur visited pos (change-dir dir))
          (recur (conj visited [new-r new-c]) [new-r new-c] dir))))))

(def answer-1 (count visited))

(defn loop?
  [the-map]
  (loop [visited #{}
         pos guard-pos
         dir [-1 0]]
    (if (visited (conj [pos] dir))
      true
      (let [new-r (+ (nth pos 0) (nth dir 0))
            new-c (+ (nth pos 1) (nth dir 1))
            new-pos [new-r new-c]]
        (if-let [new-cell (get-in the-map new-pos)]
          (if (= new-cell "#")
            (recur visited pos (change-dir dir))
            (recur (conj visited [pos dir]) new-pos dir))
          false)))))

(def answer-2
  (count (for [rc visited
               :when (and (not= rc guard-pos)
                          (loop? (assoc-in the-map rc "#")))]
           rc)))

(defn -main [& _]
  (println "Replacing first and second with nth, if-let")
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

