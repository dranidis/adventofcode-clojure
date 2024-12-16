(ns advent.2024.d16.core
  (:require
   [advent.dijkstra :refer [all-shortest-paths-from-to
                            dijkstra-shortest-distances]]
   [advent.util :refer [coords-of-symbol str->2D]]
   [clojure.set :as set]
   [clojure.test :refer [is]]))

(def example? false)
(def example "")
(def input (if example? example
               (slurp "src/advent/2024/d16/input.txt")))

;; GRID
(def grid (str->2D input))
(def start (conj (first (coords-of-symbol grid "S")) 0)) ;; orientation is EAST
(def end-set (set (let [pos (first (coords-of-symbol grid "E"))]
                    (for [orientation (range 4)]
                      (conj pos orientation)))))

(def movements [[0 1] [1 0] [0 -1] [-1 0]]) ; E S W N

;; movements
(defn forward
  [[r c orientation]]
  (let [[dr dc] (get movements orientation)]
    [(+ r dr) (+ c dc) orientation]))

(defn rotate-L
  [[r c orientation]]
  [r c (mod (dec orientation) 4)])

(defn rotate-R
  [[r c orientation]]
  [r c (mod (inc orientation) 4)])

(defn distance-neighbor-pairs
  [[r c orientation]]
  (vec (for [[new-r new-c new-orientation] [(forward [r c orientation])
                                            (rotate-L [r c orientation])
                                            (rotate-R [r c orientation])]
             :when (not= "#" (get-in grid [new-r new-c]))]
         [(if (= orientation new-orientation) 1 1000)
          [new-r new-c new-orientation]])))

(def answer-1 (apply min (map (dijkstra-shortest-distances
                               start
                               distance-neighbor-pairs) end-set)))

(defn get-visited-positions
  [path]
  (set (map (fn [[r c _]] [r c]) path)))

(def answer-2 (->> (all-shortest-paths-from-to distance-neighbor-pairs
                                               start
                                               (first end-set))
                   (map get-visited-positions)
                   (apply set/union)
                   count))


(is (= 102504 answer-1))
(is (= 535 answer-2))
(defn- -main [& _]
  (println "2024, Day 16, Part 1:" answer-1)
  (println "2024, Day 16, Part 2:" answer-2))

