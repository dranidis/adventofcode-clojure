(ns advent.2023.d1
  (:require [advent.2023.d1-input :refer [day-1-input]]
            [clojure.string :as string]))

(def sample-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defn calibration-value [s]
  (let [digits (string/replace s #"[^0-9]" "")]
    (Integer/parseInt (str (first digits) (last digits)))))

(defn total [sample-input]
  (apply + (map calibration-value (string/split-lines sample-input))))

;; Answer 1

(total day-1-input)

;; Answer 1 (alternative)
(defn digit? [char] ;; den brika digit? stin clojure.core
  (let [n (int char)]
    (and (>= n 48) (<= n 57))))

(defn total-alt [sample-input]
  (apply +
         (map (fn [l] (Integer/parseInt
                       (apply str [(first l) (last l)])))
              (for [line (string/split-lines sample-input)]
                (for [char line
                      :when (digit? char)]
                  char)))))

(total-alt day-1-input)

;;  Part 2

(def new-input "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

;; Answer 2
(defn calibrate-line [line]
  (let [numbers-and-strings
        (into []
              (map-indexed
               vector
               ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]))
        first-numbers
        (loop [n 0
               index 0]
          (if (> index (count line)) nil
              (if (> n 9)
                (recur 0 (inc index))
                (let [[number number-str] (get numbers-and-strings n)]
                  (if (or (= (str (get line index)) (str number))
                          (string/starts-with? (subs line index) number-str))
                    number
                    (recur (inc n) index))))))
        last-numbers
        (loop [n 0
               index (dec (count line))]
          (if (< index 0) nil
              (if (> n 9)
                (recur 0 (dec index))
                (let [[number number-str] (get numbers-and-strings n)]
                  (if (or (= (str (get line index)) (str number))
                          (string/starts-with? (subs line index) number-str))
                    number
                    (recur (inc n) index))))))]
    [first-numbers last-numbers]))

;; answer 2
(defn total-2 [new-input]
  (apply +
         (map
          (fn [[f l]] (Integer/parseInt (str f l)))
          (for [line (string/split-lines new-input)]
            (calibrate-line line)))))

(total-2 day-1-input)

