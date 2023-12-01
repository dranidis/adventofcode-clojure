(ns advent.2023.d1
  (:require [advent.2023.d1-input :refer [day-1-input]]
            [clojure.string :as string]))

(def sample-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defn digit? [char]
  (number? (read-string (str char))))

(defn total-alt [sample-input]
  (apply +
         (map (fn [l] (+ (* 10 (first l)) (last l)))
              (for [line (string/split-lines sample-input)]
                (for [char line :when (digit? char)]
                  (read-string (str char)))))))

;; Answer 1 
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
               ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]))]
    (loop [n 0
           index 0
           numbers []]
      (if (> index (count line))
        numbers
        (if (> n 9)
          (recur 0 (inc index) numbers)
          (let [[number number-str] (get numbers-and-strings n)]
            (if (or (= (str (get line index)) (str number))
                    (string/starts-with? (subs line index) number-str))
              (recur (inc n) index (conj numbers number))
              (recur (inc n) index numbers))))))))

(defn total-2 [new-input]
  (apply + (map (fn [digits]  (+ (* 10 (first digits)) (last digits)))
                (map calibrate-line (string/split-lines new-input)))))

;; answer 2
(total-2 day-1-input)

