(ns advent.2024.d1.core
  (:require [clojure.string :as str]))

;; (def input (slurp "src/advent/2024/d1/example.txt"))
(def input (slurp "src/advent/2024/d1/input.txt"))

(defn parse [input]
  (for [line (str/split-lines input)]
    (mapv parse-long (re-seq #"\d+" line))))

;; Part 1
(defn total-distance [input]
  (let [parsed (parse input)]
    (apply +
           (map (fn [f s] (abs (- f s)))
                (sort (mapv first parsed))
                (sort (mapv second parsed))))))

;; Part 2
(defn similarity-score [input]
  (let [parsed (parse input)]
    (apply +
           (map (fn [x]
                  (* x
                     (count (filter #(= x %) (mapv second parsed)))))
                (mapv first parsed)))))

(defn -main [& _]
  (println "Day 1, Part 1:" (total-distance input))
  (println "Day 1, Part 2:" (similarity-score input)))
