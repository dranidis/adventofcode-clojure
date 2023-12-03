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

(defn make-table [input]
  (let [lines (str/split-lines input)
        chars (map vec lines)]
    (into [] chars)))

(defn- digits->num [digits]
  (reduce (fn [acc d]
            (+ (* acc 10) d))
          0
          digits))

(defn is-symbol? [a-char]
  (and (not (Character/isDigit a-char))
       (not= a-char \.)))

(defn- coords-of-adjacent-symbols [table x y]
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

(is (= (list [[1 3] \*]) (coords-of-adjacent-symbols (make-table input) 0 2)))
(is (= (list) (coords-of-adjacent-symbols (make-table input) 0 0)))
(is (= (list) (coords-of-adjacent-symbols (make-table input) 0 1)))

(defn next-stars-coords [table x y]
  (map first (filter (fn [[_ s]] (= s \*))
                     (coords-of-adjacent-symbols table x y))))

(is (= (list [1 3]) (next-stars-coords (make-table input) 0 2)))

(defn process [input]
  (let [table (make-table input)
        total-rows (count (first table))
        total-cols (count table)]
    (loop [x 0
           y 0
           total 0
           digits []
           next-to-symbol? false
           number-and-star-coords []
           symbols #{}]
      (if (>= y total-rows)
        (if (< x (dec total-cols))
          (recur (inc x) 0 total digits  next-to-symbol? number-and-star-coords symbols)
          [total number-and-star-coords])
        (let [ch (get-in table [x y])]
          (if (Character/isDigit ch)
            ;; a number starts or continues 
            ;; accumulate the digits and the adjacent star coords
            (let [next-symbols (coords-of-adjacent-symbols table x y)
                  next-to-symbol? (or next-to-symbol? (seq next-symbols))
                  digits (conj digits (read-string (str ch)))
                  symbols (if (seq next-symbols)
                            (apply conj symbols (next-stars-coords table x y))
                            symbols)]
              (recur x (inc y) total digits next-to-symbol? number-and-star-coords symbols))
            ;; not a digit, a number has been completed; 
            ;; update the total and star-coords
            ;; reset next-to-symbol and symbols
            (let  [n (digits->num digits)
                   total (if next-to-symbol?
                           (+ total n)
                           total)
                   number-and-star-coords (if next-to-symbol?
                                            (conj number-and-star-coords [n symbols])
                                            number-and-star-coords)]
              (recur x (inc y) total [] false number-and-star-coords #{}))))))))

;; answer 1

(defn answer1 [input]
  (first (process input)))

(is (= 4361 (answer1 input)))


;; part 2 (note that process was also modified)

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

(is (= (list [1 3] [4 3] [8 5])
       (star-coords (make-table input))))

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
  ;
  )

(is (= 467835 (answer2 input)))

