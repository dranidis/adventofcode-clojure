(ns advent.2024.template
  (:require
   [advent.util :refer [str->2D str->2D-num]]
   [clojure.string :as str]))

(comment
  (def input
    "")
  ;
  )

(def input (slurp "src/advent/2024/d11/input.txt"))

;; GRID
(def grid (str->2D-num input))
(def grid (str->2D input))

(def rows (count grid))
(def cols (count (first grid)))

;; NUMBERS
(defn parse [input]
  (for [line (str/split-lines input)]
    (mapv parse-long (re-seq #"\d+" line))))

;; SECTIONS
(def order-section (first (str/split input #"\n\n")))
(def reports-section (second (str/split input #"\n\n")))

(defn parse-ord [input]
  (vec (for [line (str/split-lines input)]
         (mapv parse-long (re-seq #"\d+" line)))))