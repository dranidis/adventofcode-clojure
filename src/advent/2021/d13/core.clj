(ns advent.2021.d13.core
  (:require
   [advent.util :refer [in-vector? parse-lines-with-numbers]]
   [clojure.string :as str]))

(comment
  (def input "")
 ;
  )

(def input (slurp "src/advent/2021/d13/input.txt"))

(defn parse-instruction
  [instruction]
  [(first (re-seq #"[xy]" instruction))
   (parse-long (first (re-seq #"\d+" instruction)))])

(defn parse-lines
  [input]
  (let [all (str/split input #"\n\n")
        dots (parse-lines-with-numbers (first all))
        instructions (mapv parse-instruction (str/split-lines (second all)))]
    [dots instructions]))

(def parsed (parse-lines input))
(def paper (first parsed))
(def folds (second parsed))

(defn fold
  [first-fold paper]
  (let [fold-direction (first first-fold)
        fold-at (second first-fold)
        unfolded-part
        (filter (fn [[x y]]
                  (if (= "x" fold-direction)
                    (< x fold-at)
                    (< y fold-at))) paper)
        part-to-fold
        (filter (fn [[x y]] (if (= "x" fold-direction)
                              (> x fold-at)
                              (> y fold-at))) paper)
        folded-part
        (map (fn [[x y]]
               (if (= "x" fold-direction)
                 [(- (* 2 fold-at) x) y]
                 [x (- (* 2 fold-at) y)]))
             part-to-fold)]
    (set (concat unfolded-part folded-part))))

(def answer-1 (count (fold (first folds) paper)))

(defn fold-all
  [folds]
  (reduce (fn [a f] (fold f a)) paper folds))

(def final-points (fold-all folds))
(def maxx (apply max (map first final-points)))
(def maxy (apply max (map second final-points)))



(defn -main [& _]
  (println "Day 13, Part 1:" answer-1)
  ;; (println "Day 1, Part 2:" answer-2)

  (loop [r 0
         c 0]
    (if (> r maxy)
      nil
      (if (> c maxx)
        (do (println)
            (recur (inc r) 0))
        (do (if (final-points [c r])
              (print "#")
              (print "."))
            (recur r (inc c)))))))

;; (-main)

