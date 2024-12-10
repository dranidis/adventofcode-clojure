(ns advent.2021.d5.core
  (:require
   [advent.util :refer [transpose]]
   [clojure.string :as str]))

;; (comment
(def input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")
 ;
  ;; )

(def input (slurp "src/advent/2021/d5/input.txt"))

(defn parse-lines
  [input]
  (vec (for [line (str/split-lines input)]
         (mapv parse-long (re-seq #"\d+" line)))))

(defn extend-line
  [[x1 y1 x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (if (zero? dx)
      (vec (for [y (conj (range y1 y2 (if (pos? dy) 1 -1)) y2)]
             [x1 y]))
      (if (zero? dy)
        (vec (for [x (conj (range x1 x2 (if (pos? dx) 1 -1)) x2)]
               [x y1]))
        []))))

(def points (apply concat (map extend-line (parse-lines input))))

(defn answer
  [points]
  (loop [points points
         seen #{}
         multiple #{}]
    (if (empty? points)
      (count multiple)
      (let [p (first points)]
        (if (seen p)
          (recur (rest points) seen (conj multiple p))
          (recur (rest points) (conj seen p) multiple))))))

(defn extend-line-diagonal
  [[x1 y1 x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (if (= (abs dx) (abs dy))
      (vec (for [i (range 0 (inc (Math/abs dx)))]
             [(+ x1 (if (pos? dx) i (- i)))
              (+ y1 (if (pos? dy) i (- i)))]))

      [])))

(def diag-points (apply concat (map extend-line-diagonal (parse-lines input))))
(def all-points (concat points diag-points))


(defn -main [& _]
  (println "Day 1, Part 1:" (answer points))
  (println "Day 1, Part 2:" (answer all-points)))

(-main)

