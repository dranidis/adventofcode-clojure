(ns advent.2024.template
  (:require
   [advent.util :refer [str->2D str->2D-num]]
   [clojure.string :as str]))

(def example? false)

(def example
  "")

(def input (if example? example (slurp "src/advent/2024/d11/input.txt")))

;; GRID
(def grid (str->2D-num input))
(def grid (str->2D input))

(def rows (count grid))
(def cols (count (first grid)))

;; ONLY NUMBERS
(def parsed (parse-lines-with-numbers input))

;; SECTIONS
(def order-section (first (str/split input #"\n\n")))
(def reports-section (second (str/split input #"\n\n")))

(defn parse-ord [input]
  (vec (for [line (str/split-lines input)]
         (mapv parse-long (re-seq #"\d+" line)))))


(defn- -main [& _]
  (println "Day XX, Part 1:" answer-1)
  (println "Day XX, Part 2:" answer-2))

(-main)
