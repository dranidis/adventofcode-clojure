(ns advent.2023.d3
  (:require [advent.2023.d3-input :refer [day-3-input]]
            [clojure.string :as str]))

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

(defn make-table [input]
  (let [lines (str/split-lines input)
        chars (map vec lines)]
    (into [] chars)))

(defn is-symbol? [a-char]
  (and (not (Character/isDigit a-char))
       (not= a-char \.)))

(defn coords-of-adjacent-symbols [table x y]
  (let [total-cols (count (first table))
        total-rows (count table)]
    (filter (fn [[_ a-char]]
              (and (some? a-char) (is-symbol? a-char)))
            (map (fn [[x y]]
                   [[x y] (get-in table [x y])])
                 (filter (fn [[x y]] (and (<= 0 x total-cols)
                                          (<= 0 y total-rows)))
                         (map (fn [[dx dy]]
                                [(+ x dx) (+ y dy)])
                              [[0 1] [1 0] [0 -1] [-1 0] ;; horizontally & vertically
                               [1 1] [-1 -1] [1 -1] [-1 1] ;; diagonally
                               ]))))))

(defn next-stars-coords [table x y]
  (map first (filter (fn [[_ s]] (= s \*))
                     (coords-of-adjacent-symbols table x y))))

(defn process [input]
  (let [table (make-table input)
        total-rows (count (first table))
        total-cols (count table)]
    (loop [x 0
           y 0
           total 0
           num 0
           next-to-symbol? false
           number-and-star-coords []
           symbols #{}]
      (if (>= y total-rows)
        (if (< x (dec total-cols))
          (recur (inc x) 0 total num next-to-symbol? number-and-star-coords symbols)
          [total number-and-star-coords])
        (let [ch (get-in table [x y])]
          (if (Character/isDigit ch)
            ;; a number starts or continues 
            ;; accumulate the digits and the adjacent star coords
            (let [next-symbols (coords-of-adjacent-symbols table x y)
                  next-to-symbol? (or next-to-symbol? (seq next-symbols))
                  num' (+ (* 10 num) (Integer/parseInt (str ch)))
                  symbols (apply conj symbols (next-stars-coords table x y))]
              (recur x (inc y) total num' next-to-symbol? number-and-star-coords symbols))

            ;; not a digit, a number has been completed; 
            ;; update the total and star-coords
            ;; reset next-to-symbol and symbols
            (let  [total (if next-to-symbol?
                           (+ total num)
                           total)
                   number-and-star-coords (if next-to-symbol?
                                            (conj number-and-star-coords [num symbols])
                                            number-and-star-coords)]
              (recur x (inc y) total 0 false number-and-star-coords #{}))))))))

;; answer 1

(defn answer1 [input]
  (first (process input)))

(comment
  (answer1 input)
  ;
  )

;; part 2 (note that function process was also modified)

(defn coords-symbols [table]
  (let [width (count (first table))
        height (count table)]
    (for [x (range height)
          y (range width)]
      [[x y] (get-in table [x y])])))

(defn star-coords [table]
  (map first (filter (fn [[_ s]]
                       (= s \*))
                     (coords-symbols table))))



(defn gear-ratio [input star-coord]
  (let [list-with-number-star-coords (filter
                                      (fn [[_ set-of-star-coords]]
                                        (set-of-star-coords star-coord)) ;; contains in set
                                      (second (process input)))]
    (if (= 2 (count list-with-number-star-coords))
      (apply * (map first list-with-number-star-coords))
      0)))

(defn answer2 [input]
  (apply + (map #(gear-ratio input %)
                (star-coords (make-table input)))))

(comment
  (answer2 day-3-input)
;;
  )

