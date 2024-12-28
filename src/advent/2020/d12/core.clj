(ns advent.2020.d12.core
  (:require
   [clojure.string :as str]
   [clojure.math :as math]))

(def instructions
  (->> (str/split-lines (slurp "src/advent/2020/d12/input.txt"))
       (mapv #(let [[[_ i n]] (re-seq #"(.)(\d+)" %)]
                [i (parse-long n)]))))

;; PART 1
(println
 (loop [instructions instructions
        [x y o :as pos] [0 0 0]]
   (if (empty? instructions)
     (+ (abs x) (abs y))
     (let [[i n] (nth instructions 0)
           no (case i
                "L" (mod (+ o n) 360)
                "R" (mod (- o n) 360)
                o)
           [nx ny] (case i
                     "N" [x (+ y n)]
                     "S" [x (- y n)]
                     "E" [(+ x n) y]
                     "W" [(- x n) y]
                     "F" (case o
                           0   [(+ x n) y]
                           90  [x       (+ y n)]
                           180 [(- x n) y]
                           270 [x       (- y n)])
                     pos)]
       (recur (rest instructions) [nx ny no])))))

;; PART 2
(defn- to-left [n wx wy]
  (case n
    90 (map * [wy wx] [-1 1])
    180 (map * [wx wy] [-1 -1])
    270 (map * [wy wx] [1 -1])
    :error))

(println
 (loop [instructions instructions
        [x y :as pos]        [0 0]
        [wx wy :as waypoint] [10 1]]
   (if (empty? instructions)
     (math/round (+ (abs x) (abs y)))
     (let [[i n] (nth instructions 0)
           [nx ny] (case i
                     "F" [(+ x (* n wx))
                          (+ y (* n wy))]
                     pos)
           [nwx nwy] (case i
                       "N" [wx (+ wy n)]
                       "S" [wx (- wy n)]
                       "E" [(+ wx n) wy]
                       "W" [(- wx n) wy]
                       "L" (to-left n wx wy)
                       "R" (to-left (- 360 n) wx wy)
                       waypoint)]
       (recur (rest instructions) [nx ny] [nwx nwy])))))


