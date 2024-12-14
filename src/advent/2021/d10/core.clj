(ns advent.2021.d10.core
  (:require
   [advent.util :refer [middle-value-of-vector str->2D]]))

(comment
  (def input "")
 ;
  )

(def input (slurp "src/advent/2021/d10/input.txt"))

(def parsed (str->2D input))

(defn closes?
  [opening closing]
  (cond
    (= opening "(") (= closing ")")
    (= opening "[") (= closing "]")
    (= opening "{") (= closing "}")
    (= opening "<") (= closing ">")))

(defn check-line
  [line]
  (loop [line line
         stack (list)]
    (if (empty? line)
      stack
      (let [current (first line)]
        (if (#{"(" "[" "{" "<"} current)
          (recur (rest line) (conj stack current))
          (let [opening (first stack)]
            (if (closes? opening current)
              (recur (rest line) (rest stack))
              current)))))))

(defn score
  [char]
  (case char
    ")" 3
    "]" 57
    "}" 1197
    ">" 25137))

(defn completion-score
  [char]
  (case char
    "(" 1
    "[" 2
    "{" 3
    "<" 4))

(defn score-completed
  [stack]
  (loop [stack stack
         score 0]
    (if (empty? stack)
      score
      (let [current (first stack)
            new-score (+ (* score 5) (completion-score current))]
        (recur (rest stack) new-score)))))


(def answer-1 (apply + (map score (remove list? (map check-line parsed)))))

(def answer-2 (middle-value-of-vector (vec (sort (map score-completed (filter list? (map check-line parsed)))))))

;; (clojure.pprint/pp)

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)

