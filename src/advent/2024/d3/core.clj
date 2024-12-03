(ns advent.2024.d3.core
  (:require
   [clojure.string :as str]))

;; (def example (slurp "src/advent/2024/d3/example.txt"))
(def input (slurp "src/advent/2024/d3/input.txt"))

(defn perform-multiplication
  [multiplication]
  (let [[[_ x y]] (re-seq #"mul\((\d+),(\d+)\)" multiplication)]
    (* (parse-long x) (parse-long y))))

;; Part 1

(defn answer-1
  [input]
  (let [multiplications (re-seq #"mul\(\d+,\d+\)" (str/join input))]
    (apply + (map perform-multiplication multiplications))))

;; Part 2

(defn answer-2
  [input]
  (let [instructions (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" (str/join input))]
    (loop [instructions instructions
           acc 0
           enabled true]
      (if (empty? instructions)
        acc
        (let [instruction (ffirst instructions)]
          (case instruction
            "do()" (recur (rest instructions) acc true)
            "don't()" (recur (rest instructions) acc false)
            (if enabled
              (recur (rest instructions) (+ acc (perform-multiplication instruction)) enabled)
              (recur (rest instructions) acc enabled))))))))

(defn -main [& _]
  (println "Day 1, Part 1:" (answer-1 input))
  (println "Day 1, Part 2:" (answer-2 input)))




