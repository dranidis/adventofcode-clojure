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

(defn extrapolate-value [history op selector]
  (loop [sequences (rest (reverse (get-sequences history)))
         value 0]
    (if (empty? sequences)
      value
      (recur (rest sequences) (op (selector (first sequences))
                                  value)))))

(defn answer [input op selector]
  (apply + (map (fn [h] (extrapolate-value h op selector))
                (parse input))))

(is (= 114 (answer example + last)))
(prn (answer input + last))

;; part 2

(is (= 2 (answer example - first)))
(prn (answer input - first))