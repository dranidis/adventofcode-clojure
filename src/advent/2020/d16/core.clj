(ns advent.2020.d16.core
  (:require
   [advent.util :refer [str->nums transpose]]
   [clojure.string :as str]))

(def example? false)

(def example "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")

(def input (if example? example (slurp "src/advent/2020/d16/input.txt")))

(def sections (str/split input #"\n\n"))
(def rules
  (->> (first sections)
       (str/split-lines)
       (mapv (fn [line]
               (let [[[_ name f1 t1 f2 t2]]
                     (re-seq #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" line)]
                 [name [(mapv parse-long [f1 t1]) (mapv parse-long [f2 t2])]])))))

(def fields (->> rules (into {})))

(def ticket (->> (second sections) str->nums))
(def nearby (->> (nth sections 2)
                 str->nums))

(def all-intervals (->> rules
                        (map (fn [[_ [r1 r2]]] [r1 r2]))
                        (reduce (fn [a v] (apply conj a v)) [])))

(defn valid? [intervals n]
  (some (fn [[f t]] (<= f n t)) intervals))

;; Part 1
(->> nearby
     (remove (partial valid? all-intervals))
     (apply +) println)

;; Part 2
(def valid-tickets (->> nearby
                        (filter (partial valid? all-intervals))
                        (partition (count ticket))
                        (map vec)
                        (apply conj [ticket])))

(def field-values (transpose valid-tickets))

(def sudoku
  (map (fn [fv]
         (set (for [[n intervals] fields
                    :when (every? (partial valid? intervals) fv)]
                n)))
       field-values))


(map (fn [[i s]]
       [i (for [f s
                :when (str/starts-with? f "departure")]
            [f])])
     (map-indexed vector sudoku))





;; (defn- unique [arg1]
;;   (= (count arg1) (count (distinct arg1))))

;; (println (count (for [i0 (first sudoku)
;;                       i1 (second sudoku)
;;                       :when (not= i1 i0)
;;                       i2 (nth sudoku 2)
;;                       :when (and (not= i2 i1) (not= i2 i0))
;;                       i3 (nth sudoku 3)
;;                       :when (every? #(not= % i3) [i0 i1 i2])
;;                       i4 (nth sudoku 4)
;;                       :when (every? #(not= % i4) [i0 i1 i2 i3])
;;                       i5 (nth sudoku 5)
;;                       :when (every? #(not= % i5) [i0 i1 i2 i3 i4])
;;                       i6 (nth sudoku 6)
;;                       :when (every? #(not= % i6) [i0 i1 i2 i3 i4 i5])
;;                       i7 (nth sudoku 7)
;;                       :when (every? #(not= % i7) [i0 i1 i2 i3 i4 i5 i6])
;;                       i8 (nth sudoku 8)
;;                       :when (every? #(not= % i7) [i0 i1 i2 i3 i4 i5 i6 i7])
;;                       i9 (nth sudoku 9)
;;                       :when (every? #(not= % i7) [i0 i1 i2 i3 i4 i5 i6 i7 i9])]
;;                   [i0 i1 i2 i3 i4 i5 i6 i7 i8 i9])))


;; (for [[i [n _]] (map-indexed vector fields)
;;       :when (= 1 (count (filter #(% n) sudoku)))]
;;   [i n])

;; (count sudoku)

(def each-name-appears-in-so-many-sets
  (map
   (fn [field-name]
     [field-name (count (filter (fn [s] (s field-name)) sudoku))])
   (keys fields)))

