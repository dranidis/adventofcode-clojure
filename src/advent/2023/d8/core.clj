(ns advent.2023.d8.core
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(def example-1 (slurp "src/advent/2023/d8/example_1.txt"))
(def example-2 (slurp "src/advent/2023/d8/example_2.txt"))
(def input (slurp "src/advent/2023/d8/input.txt"))

(defn parse [input]
  (let [[instructions network] (str/split input #"\n\n")]
    [(map keyword (str/split instructions #""))
     (apply merge (for [network-line (str/split-lines network)]
                    (let [[[_ s l r]] (re-seq #"(...) = \((...), (...)\)"
                                              network-line)]
                      {s {:L l :R r}})))]))

(defn answer1 [input start]
  (let [[instructions network] (parse input)]
    (loop [inst (cycle instructions)
           state start
           steps 0]
      (if (= state "ZZZ")
        steps
        (let [state' (get-in network [state (first inst)])]
          (recur (rest inst) state' (inc steps)))))))

(is (= 2 (answer1 example-1 "AAA")))
(println "Part 1:" (answer1 input "AAA"))

;; part 2

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (abs (/ (* a b) (gcd a b))))

(defn least-common-multiplier [numbers]
  (reduce lcm numbers))

(defn period [[instructions network] start]
  (loop [inst (cycle instructions)
         state start
         steps 0]
    (if (str/ends-with? state "Z")
      steps
      (let [state' (get-in network [state (first inst)])]
        (recur (rest inst) state' (inc steps))))))

(defn states-ending-with [network letter]
  (filter (fn [state]
            (str/ends-with? state letter))
          (map first network)))

(defn answer2 [input]
  (let [[instructions network] (parse input)]
    (least-common-multiplier
     (mapv (fn [s]
             (period [instructions network] s))
           (states-ending-with network "A")))))

(is (= 6 (answer2 example-2)))
(println "Part 2:" (answer2 input))
