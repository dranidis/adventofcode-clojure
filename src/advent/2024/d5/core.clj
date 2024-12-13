(ns advent.2024.d5.core
  (:require
   [advent.util :refer [in-vector? middle-value-of-vector]]
   [clojure.string :as str]))

(def input-example "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def input (slurp "src/advent/2024/d5/input.txt"))

(def order-section (first (str/split input #"\n\n")))
(def reports-section (second (str/split input #"\n\n")))

(defn parse-ord [input]
  (vec (for [line (str/split-lines input)]
         (mapv parse-long (re-seq #"\d+" line)))))

(def page-ordering (parse-ord order-section))
(def reports (parse-ord reports-section))

(defn page-correct?
  [report page]
  ;; split report into 2 parts, one before the page and one after
  (if-let [page-idx (.indexOf report page)]
    (let [before-page (subvec report 0 page-idx)
          after-page (subvec report (inc page-idx))]
      (and (every? (fn [b] (in-vector? page-ordering [b page])) before-page)
           (every? (fn [a] (in-vector? page-ordering [page a])) after-page)))
    false))

(defn correct-report?
  [report]
  (every? (fn [page] (page-correct? report page)) report))

(def answer-1 (apply + (map middle-value-of-vector (filter correct-report? reports))))

;; Part 2

(def incorrect-reports (remove correct-report? reports))

(defn put-in-correct-order
  [incorrect-report]
  (vec (sort (fn [a b]
               (or (in-vector? page-ordering [a b])
                   (not (in-vector? page-ordering [b a]))))
             incorrect-report)))

(def answer-2 (apply + (map middle-value-of-vector (map put-in-correct-order incorrect-reports))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)
