(ns advent.2024.d1.core
  (:require [clojure.string :as str]))

;; (def example (slurp "src/advent/2024/d1/example.txt"))
(def input (slurp "src/advent/2024/d1/input.txt"))

(defn read-a-line [line]
  (mapv parse-long (re-seq #"\d+" line)))

(defn parse [input read-a-line]
  (mapv read-a-line (str/split-lines input)))

;; Part 1
(defn total-distance [input]
  (let [parsed (parse input read-a-line)]
    (apply +
           (map (fn [f s]
                  (abs (- f s)))
                (sort (mapv first parsed))
                (sort (mapv second parsed))))))

;; Part 2

(defn similarity-score [input]
  (let [parsed (parse input read-a-line)]
    (apply +
           (map (fn [x]
                  (* x
                     (count (filter #(= x %) (mapv second parsed)))))
                (mapv first parsed)))))

(defn -main [& _]
  (println "Day 1, Part 1:" (total-distance input))
  (println "Day 1, Part 2:" (similarity-score input)))
