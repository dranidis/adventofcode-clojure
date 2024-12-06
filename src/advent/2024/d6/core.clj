(ns advent.2024.d6.core
  (:require
   [clojure.string :as str]))

(comment
  (def input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")
 ;
  )

(def input (slurp "src/advent/2024/d6/input.txt"))

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
                (str c))))))

(def the-map (str->2D input))

(def guard-pos (first
                (let [rows (count the-map)
                      cols (count (first the-map))]
                  (for [r (range rows)
                        c (range cols)
                        :when (= (get-in the-map [r c]) "^")]
                    [r c]))))

(defn change-dir
  [dir]
  (case dir
    [0 1] [1 0] ;; down
    [1 0] [0 -1] ;; left
    [0 -1] [-1 0] ;; up
    [-1 0] [0 1])) ;; right

(def visited
  (loop [visited #{guard-pos}
         pos guard-pos
         dir [-1 0]]
    (let [new-r (+ (first pos) (first dir))
          new-c (+ (second pos) (second dir))
          new-cell (get-in the-map [new-r new-c])]
      (if (nil? new-cell)
        visited
        (if (= new-cell "#")
          (recur visited pos (change-dir dir))
          (recur (conj visited [new-r new-c]) [new-r new-c] dir))))))

(def answer-1 (count visited))

(defn loop?
  [the-map]
  (loop [visited #{}
         pos guard-pos
         dir [-1 0]]
    (if (visited (conj [pos] dir))
      true
      (let [new-r (+ (first pos) (first dir))
            new-c (+ (second pos) (second dir))
            new-cell (get-in the-map [new-r new-c])]
        (if (nil? new-cell)
          false
          (if (= new-cell "#")
            (recur visited pos (change-dir dir))
            (recur (conj visited [pos dir]) [new-r new-c] dir)))))))

(def answer-2
  (count (filter true?
                 (for [[r c] visited
                       :when (not= [r c] guard-pos)]
                   (loop? (assoc-in the-map [r c] "#"))))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

