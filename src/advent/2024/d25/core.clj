(ns advent.2024.d25.core
  (:require
   [advent.util :refer [str->2D transpose]]
   [clojure.string :as str]))

(def sections (str/split (slurp "src/advent/2024/d25/input.txt") #"\n\n"))

(defn lock? [grid] (every? #(= % "#") (first grid)))
(defn key? [grid] (every? #(= % "#") (nth grid (dec (count grid)))))

(defn pins
  [grid]
  (->> grid
       ((fn [g] (if (lock? g) (drop 1 g) (take (dec (count grid)) g))))
       transpose
       (map (fn [r] (filter (fn [e] (= e "#")) r)))
       (map count)))

(defn overlap? [l k] (every? #(<= % 5) (map + l k)))

(def locks (->> sections
                (map str->2D)
                (filter lock?)
                (map pins)))

(def kkeys (->> sections
                (map str->2D)
                (filter key?)
                (map pins)))

(def answer (count (for [l locks
                         k kkeys
                         :when (overlap? l k)]
                     nil)))
(println "Day 25" answer)

