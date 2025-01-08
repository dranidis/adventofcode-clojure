(ns advent.2022.d24.core
  (:require
   [advent.dijkstra :refer [A-star-shortest-distance-to-end-pred]]
   [advent.util :refer [coords-of-symbol lcm manhattan-distance str->2D]]))

(def grid (str->2D (slurp "src/advent/2022/d24/input.txt")))
(def rows (count grid))
(def cols (count (first grid)))

(defn coords-of-symbol-s
  [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [r (range rows)
          c (range cols)
          symbol [">" "v" "<" "^"]
          :when (= (get-in grid [r c]) symbol)]
      [r c symbol])))

(def walls-set (set (coords-of-symbol grid "#")))
(def Xs (coords-of-symbol-s grid))
(def icols (- cols 2))
(def irows (- rows 2))
(def start-co [0 1])
(def beyond-start-co [-1 1])
(def end-co [(dec rows) (- cols 2)])
(def beyond-end-co [rows (- cols 2)])

(defn- mov [[r c s]]
  (let [nr (dec r)
        nc (dec c)]
    (case s
      "v" [(inc (mod (inc nr) irows))
           c
           s]
      "^" [(inc (mod (dec nr) irows))
           c
           s]
      ">" [r
           (inc (mod (inc nc) icols))
           s]
      "<" [r
           (inc (mod (dec nc) icols))
           s])))

(def LCM (lcm irows icols))

(def cos-with-blizzarts-at-time
  (loop [t 0
         xs Xs
         D {}]
    (if (= t LCM)
      D
      (let [D (assoc D t (set (mapv (fn [[r c _]] [r c]) xs)))
            xs (mapv mov xs)]
        (recur (inc t) xs D)))))

(defn- neigh [[pos t]]
  (let [next-time (inc t)
        xs-set (cos-with-blizzarts-at-time (mod next-time LCM))]
    (for [d [[0 0] [0 1] [0 -1] [1 0] [-1 0]]
          :let [npos (mapv + pos d)]
          :when (and (not= npos beyond-start-co) ; do not go back beyond the start
                     (not= npos beyond-end-co) ; beyond the end
                     (not (walls-set npos)))
          :when (not (xs-set npos))]
      [npos next-time])))

(defn answers-A-star []
  (let [n-dist-fn (fn [n] (map (fn [nn] [1 nn]) (neigh n)))
        m-dist-to-start (fn [[pos _]] (manhattan-distance pos start-co))
        m-dist-to-end (fn [[pos _]] (manhattan-distance pos end-co))
        end? (fn [[n _]] (= n end-co))
        start? (fn [[n _]] (= n start-co))

        time-to-end (A-star-shortest-distance-to-end-pred
                     [start-co 0]
                     end?
                     n-dist-fn
                     m-dist-to-end)
        _ (println "ANS 1 to end" time-to-end)
        time-to-start (A-star-shortest-distance-to-end-pred
                       [end-co time-to-end]
                       start?
                       n-dist-fn
                       m-dist-to-start)
        time-to-end-again (A-star-shortest-distance-to-end-pred
                           [start-co (+ time-to-end time-to-start)]
                           end?
                           n-dist-fn
                           m-dist-to-end)]
    (println "ANS 2: to end, to start and to end" (+ time-to-end time-to-start time-to-end-again))))

(time (answers-A-star))