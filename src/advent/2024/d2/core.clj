(ns advent.2024.d2.core
  (:require
   [clojure.string :as str]))

(def example (slurp "src/advent/2024/d2/example.txt"))
(def input (slurp "src/advent/2024/d2/input.txt"))

(defn parse [input]
  (for [line (str/split-lines input)]
    (mapv parse-long (re-seq #"\d+" line))))

;; Part 1

(defn safe-report?
  [report]
  (let [differences-between-levels
        (-> report
            (#(partition 2 1 %))
            (#(mapv (fn [[level-1 level-2]] (- level-1 level-2)) %)))]
    (and (every? (fn [x] (not= 0 x)) differences-between-levels)
         (or (every? (fn [x] (<= 1 x 3)) differences-between-levels)
             (every? (fn [x] (>= -1 x -3)) differences-between-levels)))))

(defn answer-1
  [input]
  (count (filter true? (map safe-report? (parse input)))))


;; Part 2

(defn safe-2-report?
  [report]
  (let [different-reports-with-max-one-level-removed
        (concat [report]
                (for [level-to-remove (range (count report))]
                  (vec (concat (subvec report 0 level-to-remove)
                               (subvec report (inc level-to-remove))))))]
    (some true? (map safe-report? different-reports-with-max-one-level-removed))))

(defn answer-2
  [input]
  (count (filter true? (map safe-2-report? (parse input)))))


(defn -main [& _]
  (println "Day 1, Part 1:" (answer-1 input))
  (println "Day 1, Part 2:" (answer-2 input)))

(some? [true])




