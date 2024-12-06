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

(defn find-guard
  [the-map]
  (let [rows (count the-map)
        cols (count (first the-map))]
    (for [r (range rows)
          c (range cols)
          :when (= (get-in the-map [r c]) "^")]
      [r c])))

(def guard-pos (first (find-guard the-map)))

(defn change-dir
  [dir]
  (cond
    (= dir [0 1]) [1 0] ;; down
    (= dir [1 0]) [0 -1] ;; left
    (= dir [0 -1]) [-1 0] ;; up
    (= dir [-1 0]) [0 1])) ;; right

(defn- at-direction-there-is-a-visited
  [the-map visited dir pos]
  (loop [r (first pos)
         c (second pos)
         dir dir
         visited visited]
    (if (nil? (get-in the-map [r c]))
      false
      (let [new-r (+ r (first dir))
            new-c (+ c (second dir))]
        (if (= "#" (get-in the-map [new-r new-c]))
          (recur r c (change-dir dir) visited)
          (if (visited [new-r new-c dir])
            true
            (recur new-r new-c dir (conj visited [new-r new-c dir]))))))))

(comment
  (loop [visited #{(conj guard-pos [-1 0])}
         pos guard-pos
         dir [-1 0]
         changed-dir 1
         extra-obs #{}]
  ;; (println visited pos dir extra)
    (let [new-r (+ (first pos) (first dir))
          new-c (+ (second pos) (second dir))]
      (if (nil? (get-in the-map [new-r new-c]))
        [(count (set (map (fn [[r c _]] [r c]) visited)))
         (count extra-obs)]
        (let [new-extra-obs (if (and (not (extra-obs [new-r new-c]))
                                     (at-direction-there-is-a-visited
                                      (assoc-in the-map [new-r new-c] "#")
                                      visited (change-dir dir) pos))
                              (conj extra-obs [new-r new-c])
                              extra-obs)]
          (if (= (get-in the-map [new-r new-c]) "#")
            (recur visited pos (change-dir dir) (inc changed-dir) new-extra-obs)
            (recur (conj visited [new-r new-c dir]) [new-r new-c] dir changed-dir new-extra-obs))))))
  ;
  )

(def visited
  (loop [visited #{guard-pos}
         pos guard-pos
         dir [-1 0]
         changed-dir 1]
    (let [new-r (+ (first pos) (first dir))
          new-c (+ (second pos) (second dir))]
      (if (nil? (get-in the-map [new-r new-c]))
        visited
        (if (= (get-in the-map [new-r new-c]) "#")
          (recur visited pos (change-dir dir) (inc changed-dir))
          (recur (conj visited [new-r new-c]) [new-r new-c] dir changed-dir))))))

(def answer-1 (count visited))

(defn try-map
  [the-map]
  (loop [visited #{}
         pos guard-pos
         dir [-1 0]
         changed-dir 1
         extra 0]
    (let [new-r (+ (first pos) (first dir))
          new-c (+ (second pos) (second dir))]
      (if (visited (conj [pos] dir))
        nil
        (if (nil? (get-in the-map [new-r new-c]))
          [(count (set (map (fn [[r c _]] [r c]) visited)))
           extra]
          (if (= (get-in the-map [new-r new-c]) "#")
            (recur visited pos (change-dir dir) (inc changed-dir) extra)
            (recur (conj visited [pos dir]) [new-r new-c] dir changed-dir extra)))))))

(def answer-2
  (count (filter nil?
                 (for [[r c] visited
                       :when (not= [r c] guard-pos)
                       :let [new-map (assoc-in the-map [r c] "#")]]
                   (try-map new-map)))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

