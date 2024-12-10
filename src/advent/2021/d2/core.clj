(ns advent.2021.d2.core
  (:require
   [clojure.string :as str]))

(comment
  (def input "forward 5
down 5
forward 8
up 3
down 8
forward 2")
 ;
  )

(def input (slurp "src/advent/2021/d2/input.txt"))

(defn parse-lines [input]
  (vec (for [line (str/split-lines input)]
         (let [[cmd number] (str/split line #" ")]
           [cmd (parse-long number)]))))

(def answer-1 (loop [x 0
                     y 0
                     cmd (parse-lines input)]
                (if (empty? cmd)
                  (* x y)
                  (let [[c n] (first cmd)]
                    (case c
                      "forward" (recur (+ x n) y (rest cmd))
                      "up" (recur x (- y n) (rest cmd))
                      "down" (recur x (+ y n) (rest cmd)))))))

(def answer-2 (loop [x 0
                     y 0
                     aim 0
                     cmd (parse-lines input)]
                (if (empty? cmd)
                  (* x y)
                  (let [[c n] (first cmd)]
                    (case c
                      "forward" (recur (+ x n) (+ y (* aim n)) aim (rest cmd))
                      "up" (recur x y (- aim n) (rest cmd))
                      "down" (recur x y (+ aim n) (rest cmd)))))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)
