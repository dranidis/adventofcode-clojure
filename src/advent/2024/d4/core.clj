(ns advent.2024.d4.core
  (:require
   [advent.2023.d23.core :refer [str->2D]]))

;; (def input "MMMSXXMASM
;; MSAMXMSMSA
;; AMXSXMAAMM
;; MSAMASMSMX
;; XMASAMXAMM
;; XXAMMXXAMA
;; SMSMSASXSS
;; SAXAMASAAA
;; MAMMMXMMMM
;; MXMXAXMASX")
(def input (slurp "src/advent/2024/d4/input.txt"))

;; Part 1

(defn answer-1
  [input]
  (let [grid (str->2D input)]
    (count (for [r (range (count grid))
                 c (range (count (first grid)))
                 [dx dy] (map vec [[-1 0] [0 1] [1 0] [0 -1] [1 1] [-1 1] [1 -1] [-1 -1]])
                 :when (= (get-in grid [r c]) "X")
                 :when (= (get-in grid [(+ r dx) (+ c dy)]) "M")
                 :when (= (get-in grid [(+ r (* 2 dx)) (+ c (* 2 dy))]) "A")
                 :when (= (get-in grid [(+ r (* 3 dx)) (+ c (* 3 dy))]) "S")]
             [r c dx dy]))))

;; Part 2

(defn answer-2
  [input]
  (let [grid (str->2D input)]
    (count
     (for [r (range (count grid))
           c (range (count (first grid)))
           :when (= (get-in grid [r c]) "A")
           :let [top-l (get-in grid [(dec r) (dec c)])
                 bottom-r (get-in grid [(inc r) (inc c)])
                 bottom-l (get-in grid [(inc r) (dec c)])
                 top-r (get-in grid [(dec r) (inc c)])]
           :when (every? #{"M" "S"} [top-l bottom-r top-r bottom-l])
           :when (and (not= top-l bottom-r)
                      (not= top-r bottom-l))]
       [r c]))))

(defn -main [& _]
  (println "Day 1, Part 1:" (answer-1 input))
  (println "Day 1, Part 2:" (answer-2 input)))
