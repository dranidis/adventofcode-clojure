(ns advent.2021.d4.core
  (:require
   [advent.util :refer [transpose]]
   [clojure.string :as str]))

;; (comment
(def input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")
 ;
  ;; )

(def input (slurp "src/advent/2021/d4/input.txt"))

(defn parse-board
  [board]
  (vec (for [line (str/split-lines board)]
         (mapv parse-long (str/split (str/trim line) #"  *")))))

(defn parse-lines
  [input]
  (let [all (str/split input #"\n\n")
        drawn (mapv parse-long (str/split (first all) #","))
        boards (mapv
                parse-board
                (rest all))]
    [drawn boards]))

(def drawn (first (parse-lines input)))
(def boards (second (parse-lines input)))

;; (pprint/pprint boards)

(defn mark-number
  [num board]
  (mapv (fn [row]
          (mapv (fn [cell]
                  (if (= num cell)
                    nil
                    cell))
                row))
        board))

(defn win?
  [board]
  (or (some (partial every? nil?) board)
      (some (partial every? nil?) (transpose board))))

(def answer-1
  (loop [drawn drawn
         boards boards]
    ;; (println "\n\nNUMBER" (first drawn))
    ;; (pprint/pprint boards)
    (if (empty? drawn)
      nil
      (let [num (first drawn)
            new-boards (mapv (partial mark-number num) boards)
            winned (filter win? new-boards)]
        (if (= 1 (count winned))
          [num (* num (apply + (remove nil? (flatten (first winned)))))]
          (recur (rest drawn) new-boards))))))

(def answer-2
  (loop [drawn drawn
         boards boards]
    ;; (println "\n\nNUMBER" (first drawn))
    ;; (pprint/pprint boards)
    (if (empty? drawn)
      nil
      (let [num (first drawn)
            new-boards (mapv (partial mark-number num) boards)
            remaining (remove win? new-boards)]
        (if (empty? remaining)
          [num (* num (apply + (remove nil? (flatten (first new-boards)))))]
          (recur (rest drawn) remaining))))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)

