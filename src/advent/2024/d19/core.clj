(ns advent.2024.d19.core
  (:require [clojure.string :as str]))

(def example? true)
(def example "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(def input (if example? example (slurp "src/advent/2024/d19/input.txt")))

(def sections (str/split input #"\n\n"))
(def patterns (map str/trim (str/split (first sections) #",")))
(def towels (str/split (second sections) #"\n"))

(def ways-count
  (memoize (fn [s]
             (if (empty? s)
               1
               (->> patterns
                    (filter #(str/starts-with? s %))
                    (map #(apply str (drop (count %) s)))
                    (map (partial ways-count))
                    (apply +))))))

(defn- -main [& _]
  (println "Day 19, Part 1:"
           (count (filter #(> % 0) (map ways-count towels))))
  (println "Day 19, Part 2:"
           (apply + (map ways-count towels))))
