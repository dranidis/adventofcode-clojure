(ns advent.2023.d1.core
  (:require
   [clojure.string :as string]))

(def example-1 (slurp "src/advent/2023/d1/example_1.txt"))
(def input (slurp "src/advent/2023/d1/input.txt"))

(defn get-number [digits]
  (parse-long (apply str [(first digits) (last digits)])))

(defn total-1 [input]
  (apply +
         (for [line (string/split-lines input)]
           (let [digits (re-seq #"\d" line)]
             (get-number digits)))))

;;  Part 2

(def example-2 (slurp "src/advent/2023/d1/example_2.txt"))

(def numbers-and-strings
  (vec (map-indexed
        vector
        ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])))

(defn calibrate-line [line]
  (loop [num-str-index 0
         index 0
         numbers []]
    (if (> index (count line))
      numbers
      (if (> num-str-index 9)
        (recur 0 (inc index) numbers)
        (let [[number number-str] (get numbers-and-strings num-str-index)]
          (if (or (= (parse-long (str (get line index))) number)
                  (string/starts-with? (subs line index) number-str))
            (recur (inc num-str-index) index (conj numbers number))
            (recur (inc num-str-index) index numbers)))))))

(defn total-2 [new-input]
  (apply + (map get-number
                (map calibrate-line
                     (string/split-lines new-input)))))

(defn -main [& _]
  (println "Day 1, Part 1:" (total-1 input))
  (println "Day 1, Part 2:" (total-2 input)))
