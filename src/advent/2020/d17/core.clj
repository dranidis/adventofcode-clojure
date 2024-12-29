(ns advent.2020.d17.core
  (:require
   [advent.util :refer [coords-of-symbol draw-grid-from-points str->2D]]
   [clojure.set :as set]))

(def example? false)

(def example ".#.
..#
###")

(def input (if example? example (slurp "src/advent/2020/d17/input.txt")))

;; GRID
;; (def grid (str->2D-num input))
(def grid (str->2D input))

(def Xs (->> (coords-of-symbol grid "#")
             (map (fn [[x y]] [x y 0]))
             set))

(def ds (for [x [1 0 -1]
              y [1 0 -1]
              z [1 0 -1]
              :when (not= [0 0 0] [x y z])]
          [x y z]))

(defn neighbors [ds pos]
  (set (for [d ds]
         (map + pos d))))

(defn active-neigh-count [ds xyzs point]
  (count (set/intersection
          (neighbors ds point)
          xyzs)))

(defn upd-cycle [ds xyzs]
  ;; (println xyzs)
  ;; (println "z=-1" (zslice xyzs -1))
  ;; (println "z=0" (zslice xyzs 0))
  ;; (println "z=1" (zslice xyzs 1))
  (let [remain
        (set (for [point xyzs
                   :when (#{2 3} (active-neigh-count ds xyzs point))]
               point))
        become-active
        (set (for [point (set (mapcat (partial neighbors ds) xyzs))
                   :when (not (xyzs point))
                   :when (= 3 (active-neigh-count ds xyzs point))]
               point))]
    (set/union remain become-active)))

;; part 1
(->>  Xs
      (iterate #(upd-cycle ds %))
      (take 7)
      last
      count
      println)

;; part 2
(def X4s (->> (coords-of-symbol grid "#")
              (map (fn [[x y]] [x y 0 0]))
              set))

(def ds4 (for [x [1 0 -1]
               y [1 0 -1]
               z [1 0 -1]
               w [1 0 -1]
               :when (not= [0 0 0 0] [x y z w])]
           [x y z w]))

(->>  X4s
      (iterate #(upd-cycle ds4 %))
      (take 7)
      last
      count
      println)