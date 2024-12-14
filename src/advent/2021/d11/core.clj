(ns advent.2021.d11.core
  (:require
   [advent.util :refer [str->2D-num]]))

(comment
  (def input "")
 ;
  )

(def input (slurp "src/advent/2021/d11/input.txt"))

(def grid (str->2D-num input))
(def rows (count grid))
(def cols (count (first grid)))

(defn get-flashing
  [grid]
  (for [r (range rows)
        c (range cols)
        :when (> (get-in grid [r c]) 9)]
    [r c]))

(defn energy-increase
  [grid [row col]]
  (let [neighbors (for
                   [dir [[0 1] [1 0] [0 -1] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1]]
                    :let [nr (+ row (first dir))
                          nc (+ col (second dir))]
                    :when (and (>= nr 0) (< nr rows)
                               (>= nc 0) (< nc cols))
                    :when (not= 0 (get-in grid [nr nc]))]
                    [nr nc])
        new-grid  (reduce (fn [grid [r c]]
                            (update-in grid [r c] inc))
                          grid
                          neighbors)
        new-grid (assoc-in new-grid [row col] 0)]
    [new-grid (get-flashing new-grid)]))

(defn flashing
  [grid]
  (let [flashing (get-flashing grid)]
    (loop [grid grid
           flashing flashing
           flashed #{}]
      (if (empty? flashing)
        [grid (count flashed)]
        (let [fl (first flashing)]
          (if (flashed fl)
            (recur grid (rest flashing) flashed)
            (let [[new-grid new-flashing] (energy-increase grid fl)
                  flashed (conj flashed fl)
                  flashing (concat (rest flashing) new-flashing)]
              (recur new-grid flashing flashed))))))))

(def answer-1
  (loop [grid grid
         step 0
         flashes 0]
    (if (= step 100)
      flashes
      (let [new-grid (mapv #(mapv inc %) grid)
            [update-grid step-flashes] (flashing new-grid)]
        (recur update-grid (inc step) (+ flashes step-flashes))))))

(apply + (flatten grid))
(def answer-2
  (loop [grid grid
         step 0
         flashes 0]
    (if (= 0 (apply + (flatten grid)))
      step
      (let [new-grid (mapv #(mapv inc %) grid)
            [update-grid step-flashes] (flashing new-grid)]
        (recur update-grid (inc step) (+ flashes step-flashes))))))

(defn -main [& _]
  (println "Day 11, Part 1:" answer-1)
  (println "Day 11, Part 2:" answer-2))

