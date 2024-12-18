(ns advent.2024.d18.core
  (:require
   [advent.dijkstra :refer [dijkstra-shortest-distances]]
   [advent.util :refer [flood-fill-rc parse-lines-with-numbers]]))

(def example? false)
(def example "")

(def input (if example? example (slurp "src/advent/2024/d18/input.txt")))
(def parsed (parse-lines-with-numbers input))
(def maxx (if example? 6 70))
(def start [0 0])
(def end [maxx maxx])
(def fallen (if example? 12 1024))

(defn corrupted
  [fallen]
  (set (take  fallen parsed)))

(defn neighbors-distances
  [fallen]
  (fn [[x y]]
    (for [[dx dy] [[0 1] [1 0] [0 -1] [-1 0]]
          :let [nx (+ x dx)
                ny (+ y dy)]
          :when (and (<= 0 nx maxx)
                     (<= 0 ny maxx))
          :when (not ((corrupted fallen) [nx ny]))]
      [1 [nx ny]])))

(defn neighbors
  [fallen]
  (fn [[x y]]
    (vec (for [[dx dy] [[0 1] [1 0] [0 -1] [-1 0]]
               :let [nx (+ x dx)
                     ny (+ y dy)]
               :when (and (<= 0 nx maxx)
                          (<= 0 ny maxx))
               :when (not ((corrupted fallen) [nx ny]))]
           [nx ny]))))


(defn distances
  [fallen]
  (dijkstra-shortest-distances start
                               (neighbors-distances fallen)))

(def answer-1 ((distances fallen) end))

;; Part 2

(defn set-of-points-from-the-end
  [fallen]
  (flood-fill-rc (neighbors fallen) end))

(def answer-2
  (get parsed
       (dec (loop [from fallen
                   till (count parsed)]
              (if (= from till)
                from
                (let [mid (+ from (quot (- till from) 2))
                      connected? ((set-of-points-from-the-end mid) start)]
                  (if connected?
                    (recur (inc mid) till)
                    (recur from mid))))))))

(defn- -main [& _]
  (println "Day XX, Part 1:" answer-1)
  (println "Day XX, Part 2:" answer-2))
