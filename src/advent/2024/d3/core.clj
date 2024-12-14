(ns advent.2024.d3.core
  (:require
   [clojure.string :as str]))

;; (def input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def input (slurp "src/advent/2024/d3/input.txt"))

(defn perform-multiplication
  [multiplication]
  (apply * (map parse-long (re-seq #"\d+" multiplication))))

;; Part 1

(defn answer-1
  [input]
  (let [multiplications (re-seq #"mul\(\d+,\d+\)" (str/join input))]
    (apply + (map perform-multiplication multiplications))))

;; Part 2

(defn answer-2
  [input]
  (let [instructions (re-seq #"mul\(\d+,\d+\)|do\(\)|don't\(\)" (str/join input))]
    (loop [instructions instructions
           acc 0
           enabled true]
      (if (empty? instructions)
        acc
        (let [instruction (first instructions)]
          (case instruction
            "do()" (recur (rest instructions) acc true)
            "don't()" (recur (rest instructions) acc false)
            (recur (rest instructions)
                   (if enabled
                     (+ acc (perform-multiplication instruction))
                     acc) enabled)))))))

(defn -main [& _]
  (println "Day 1, Part 1:" (answer-1 input))
  (println "Day 1, Part 2:" (answer-2 input)))

