(ns advent.2023.d11.core
  (:require [clojure.string :as str]))

(def example (slurp "src/advent/2023/d11/example.txt"))
(def input (slurp "src/advent/2023/d11/input.txt"))

(defn parse [input]
  (vec (for [line (str/split-lines input)]
         (vec line))))

(defn transpose [m]
  (apply mapv vector m))

(defn expand-rows [map-rows]
  (let [num-rows (count (first map-rows))]
    (vec (partition num-rows
                    (flatten (mapv (fn [map-row]
                                     (if (every? #(= \. %) map-row)
                                       [map-row map-row]
                                       map-row))
                                   map-rows))))))

(defn expand-map [galaxy-map]
  (transpose (expand-rows (transpose (expand-rows galaxy-map)))))

(defn manhattan-distance [[s1 s2] [b1 b2]]
  (+ (abs (- s1 b1)) (abs (- s2 b2))))

(defn- all-galaxies-coords [the-galaxies]
  (let [max-row (count the-galaxies)
        max-col (count (first the-galaxies))]
    (loop [col 0
           row 0
           galaxies []]
      (if (>= row max-row)
        galaxies
        (if (>= col max-col)
          (recur 0 (inc row) galaxies)
          (if (= \# (get-in the-galaxies [row col]))
            (recur (inc col) row (conj galaxies [col row]))
            (recur (inc col) row galaxies)))))))

(defn total-all-pair-distances [coords]
  (quot (apply + (for [g1 coords
                       g2 coords
                       :when (not= g1 g2)]
                   (manhattan-distance g1 g2)))
        2))

(defn answer-1 [input]
  (total-all-pair-distances (all-galaxies-coords (expand-map (parse input)))))

;; part 2

(defn rows-to-add [the-galaxies-map expansion-by]
  (map (fn [row]
         (if (every? #(= \. %) row)
           expansion-by
           0))
       the-galaxies-map))

(defn calc-extra-distances-rows [the-galaxies-map expansion-by]
  (first (reduce (fn [[acc total] v]
                   (let [total (+ total v)
                         v total]
                     [(conj acc v) total]))
                 [[] 0]
                 (rows-to-add the-galaxies-map expansion-by))))

(defn calc-extra-distances-cols [the-galaxies-map expansion-by]
  (calc-extra-distances-rows (transpose the-galaxies-map) expansion-by))

(defn expand-coords [input expansion-by]
  (let [extra-rows (calc-extra-distances-rows (parse input) expansion-by)
        extra-cols (calc-extra-distances-cols (parse input) expansion-by)]
    (map (fn [[x y]]
           [(+ x (nth extra-cols x))
            (+ y (nth extra-rows y))])
         (all-galaxies-coords (parse input)))))

(defn answer-2 [input expansion-by]
  (total-all-pair-distances (expand-coords input (dec expansion-by))))

(defn -main [& _]
  (println "Day 1, Part 1:" (answer-1 input))
  (println "Day 1, Part 2:" (answer-2 input 1000000)))