(ns advent.2023.d3
  (:require [advent.2023.d3-input :refer [day-3-input]]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn characters [input]
  (let [lines (for [line (str/split-lines input)]
                line)
        w (count (first lines))
        chars (map vec lines)]
    (into [] chars)))

(def table (characters input))
table

(defn- digits->num [digits]
  (reduce (fn [acc d]
            (+ (* acc 10) d))
          0
          digits))

(digits->num [4 6 7])

(defn digit? [char] ;; den brika digit? stin clojure.core
;;   (println "DIGIT? " char)
  (let [n (int char)]
    (and (>= n 48) (<= n 57))))

(defn- is-next-to-symbol? [table x y]
  (let [width (count (first table))
        height (count table)]
    (some (fn [s]
            ;; (println "SOME" s "END")
            (and (not (nil? s))
                 (not (digit? s))
                 (not= s \.)))
          (map (fn [[x y]]

                ;;  (str (get-in table [x y]))

                 (get-in table [x y]))
               (filter (fn [[x y]] (and (<= 0  x  width)
                                        (<= 0 y height)))
                       (map (fn [[dx dy]]
                              [(+ x dx) (+ y dy)])
                            [[0 1] [1 0] [0 -1] [-1 0]
                             [1 1] [-1 -1] [1 -1] [-1 1]]))))))


(is (not (is-next-to-symbol? table 0 0)))
(is (not (is-next-to-symbol? table 0 1)))
(is (is-next-to-symbol? table 0 2))



(defn parse [input]
  (let [table (characters input)
        width (count (first table))
        height (count table)]
    (loop [x 0
           y 0
           total 0
           digits []
           next-to-symbol? false
           gear-numbers []]
      (println "AT STATE " "x" x "y" y total digits next-to-symbol? gear-numbers)
      (if (>= y width)
        (if (< x (dec height))
          (recur (inc x) 0 total digits  next-to-symbol? gear-numbers)
          total)
        (let [ch (str (get-in table [x y]))]
          (if (digit? (first ch))
            (let [symbol? (or next-to-symbol? (is-next-to-symbol? table x y))]
              (recur x
                     (inc y)
                     total
                     (conj digits (read-string ch))
                     symbol?
                     gear-numbers))
            ;; not a digit
            (let  [n (digits->num digits)
                   total (if next-to-symbol?
                           (+ total n)
                           total)
                   gear-numbers (if next-to-symbol?
                                  (conj gear-numbers n)
                                  gear-numbers)]
              (recur x
                     (inc y)
                     total
                     []
                     false
                     gear-numbers))))))))



(parse input)

;; (parse day-3-input)