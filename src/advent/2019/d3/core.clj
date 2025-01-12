(ns advent.2019.d3.core
  (:require
   [advent.util :refer [manhattan-distance]]
   [clojure.set :as set]
   [clojure.string :as str]))

(def ls (->> (slurp "src/advent/2019/d3/input.txt")
             str/split-lines
             (mapv (fn [l]
                     (let [xx (str/split l #",")]
                       (mapv #(let [[[_ m n]] (re-seq #"(.)(\d+)" %)]
                                [m (parse-long n)])
                             xx))))))

(def wire1 (first ls))
(def wire2 (second ls))

(defn points [wire1]
  (loop [wire1 wire1
         [x y] [0 0]
         p []]
    (if (empty? wire1)
      p
      (let [[dir n] (first wire1)
            [xrange yrange pos]
            (case dir
              "U" [[0] (range 1 (inc n)) [x (+ y n)]]
              "D" [[0] (range -1 (dec (- n)) -1) [x (- y n)]]
              "R" [(range 1 (inc n)) [0] [(+ x n) y]]
              "L" [(range -1 (dec (- n)) -1) [0] [(- x n) y]])
            pp (for [dx xrange
                     dy yrange]
                 [(+ x dx) (+ y dy)])]
        (recur (rest wire1) pos (apply conj p pp))))))

(def p1 (points wire1))
(def p2 (points wire2))

(def intersections (set/intersection (set p1) (set p2)))

(println "ANS 1: "
         (->> intersections
              (map (partial manhattan-distance [0 0]))
              (apply min)))

(println "ANS 2: "
         (->> (for [i intersections
                    :let [s1 (inc (.indexOf p1 i))
                          s2 (inc (.indexOf p2 i))
                          s (+ s1 s2)]]
                s)
              (apply min)))