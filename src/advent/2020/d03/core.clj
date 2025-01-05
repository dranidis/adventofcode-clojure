(ns advent.2020.d03.core
  (:require
   [advent.util :refer [str->2D]]))

(def grid (str->2D (slurp "src/advent/2020/d03/input.txt")))
(def rows (count grid))
(def cols (count (first grid)))

(defn slope
  [dr dc] (loop [r 0
                 c 0
                 t 0]
            (if (>= r rows)
              t
              (let [t (if (= "#" (get-in grid [r c])) (inc t) t)]
                (recur (+ r dr) (mod (+ c dc) cols) t)))))

(println (slope 1 3))

(println (* (slope 1 1) (slope 1 3) (slope 1 5) (slope 1 7) (slope 2 1)))
