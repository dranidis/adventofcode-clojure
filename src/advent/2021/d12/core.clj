(ns advent.2021.d12.core
  (:require
   [advent.util :refer [in-vector?]]
   [clojure.string :as str]))

(comment
  (def input "")
 ;
  )

(def input (slurp "src/advent/2021/d12/input.txt"))

(defn parse-line
  [line]
  (let [[from to] (str/split line #"-")]
    [from to]))

(defn parse-lines
  [input]
  (map parse-line (str/split-lines input)))

(def parsed (parse-lines input))

(def connections (concat (mapv (fn [[from to]] [to from]) parsed) parsed))
(def nodes (set (mapcat identity connections)))

(def next-map (reduce
               (fn [m n]
                 (assoc m n
                        (mapv second
                              (filter #(and (= n (first %)) (not= "start" (second %))) connections))))
               {}
               nodes))

(defn uppercase? [s]
  (= s (clojure.string/upper-case s)))

;; BFS
(defn find-paths
  "Find all paths from start to end.
   Upper case nodes can be visited multiple times.
   Lower case nodes can be visited only once."
  [start end]
  (loop [queue [[start]]
         paths []]
    (if (empty? queue)
      paths
      (let [path (first queue)
            current (last path)]
        (if (= current end)
          (recur (rest queue) (conj paths path))
          (let [nexts (filter #(or (uppercase? %) (not (in-vector? path %))) (next-map current))
                new-paths (map #(conj path %) nexts)]
            (recur (concat new-paths (rest queue)) paths)))))))

(def answer-1 (count (find-paths "start" "end")))

(defn find-paths-with-extra-small-cave
  "Find all paths from start to end.
   Upper case nodes can be visited multiple times.
   A SINGLE lower case node can be visited at most TWICE.
   All other lower case nodes can be visited only once."
  "The only difference is in the filter function for next nodes"
  [start end]
  (loop [queue [[start]]
         paths []]
    (if (empty? queue)
      paths
      (let [path (first queue)
            current (last path)]
        (if (= current end)
          (recur (rest queue) (conj paths path))
          (let [nexts (filter
                       #(or (uppercase? %)
                            (not (in-vector? path %))
                            (and (= 1 (count (filter (fn [n] (= % n)) path)))
                                 (= (count (set (remove uppercase? path)))
                                    (count (remove uppercase? path)))))
                       (next-map current))
                new-paths (map #(conj path %) nexts)]
            (recur (concat new-paths (rest queue)) paths)))))))

(def answer-2 (count (find-paths-with-extra-small-cave "start" "end")))

(comment
  (def path ["A" "a" "b" "A" "a"])
  (= (count (set (remove uppercase? path)))
     (count (remove uppercase? path)))
;
  )
(defn -main [& _]
  (println "Day 12, Part 1:" answer-1)
  (println "Day 12, Part 2:" answer-2))
