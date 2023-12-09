(ns advent.2023.d9.core
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

;; --- Day 9: Mirage Maintenance ---

(def example (slurp "src/advent/2023/d9/example.txt"))
(def input (slurp "src/advent/2023/d9/input.txt"))

(defn parse [input]
  (vec (for [line (str/split-lines input)]
         (read-string (str "[" line "]")))))

(defn difference-at-each-step [history]
  (mapv (fn [[x y]] (- y x))
        (partition 2 1 history)))

(defn get-sequences [history]
  (loop [history history
         sequences [history]]
    (if (every? zero? history)
      sequences
      (let [h (difference-at-each-step history)]
        (recur h (conj sequences h))))))

(defn extrapolate-value [history]
  (loop [sequences (rest (reverse (get-sequences history)))
         value 0]
    (if (empty? sequences)
      value
      (recur (rest sequences) (+ (last (first sequences))
                                 value)))))

(defn answer1 [input]
  (apply + (map extrapolate-value (parse input))))

(is (= 114 (answer1 example)))
(is (= 1637452029 (answer1 input)))

;; part 2

(defn extrapolate-value-2 [history]
  (loop [sequences (rest (reverse (get-sequences history)))
         value 0]
    (if (empty? sequences)
      value
      (recur (rest sequences) (- (first (first sequences))
                                 value)))))

(defn answer2 [input]
  (apply + (map extrapolate-value-2 (parse input))))

(is (= 2 (answer2 example)))
(is (= 908 (answer2 input)))
