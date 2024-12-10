(ns advent.2021.d17.core
  (:require
   [advent.util :refer [str->2D]]
   [clojure.string :as str]))

(def example? false)

(def input (if example? "2333133121414131402"
               (slurp "src/advent/2024/d10
              /input.txt")))


;; (defn- parse [input]
;;   (mapv parse-long (str/split input #"")))



;; (defn parse [input]
;;   (for [line (str/split-lines input)]
;;     (mapv parse-long (re-seq #"\d+" line))))



;; (def grid (str->2D input))
;; (def rows (count grid))
;; (def cols (count (first grid)))


;; (defn- -main [& _]
;;   (println "Day 9, Part 1:" answer-1)
;;   (println "Day 9, Part 2:" answer-2))


(defn add
  [a b]
  [a b])

(add 1 2)


(add [[[[4 3] 4] 4] [7 [[8 4] 9]]] [1 1])

(defn magnitude
  [v]
  (cond (number? v) v
        (vector? v) (let [mv (mapv magnitude v)]
                      (+ (* 3 (first mv)) (* 2 (second mv))))))

(vector? [[1 2] [[3 4] 5]])
(map magnitude [[1 2] [[3 4] 5]])
(magnitude 1)
(magnitude [9 1])
(magnitude [[1 2] [[3 4] 5]])