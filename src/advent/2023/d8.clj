(ns advent.2023.d8
  (:require [advent.2023.d8-input :refer [day-8-input]]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn parse [input]
  (let [[instructions network] (str/split input #"\n\n")]
    [(map keyword (str/split instructions #""))
     (reduce (fn [acc [from to]]
               (assoc acc from to))
             {}
             (for [network-line (str/split-lines network)]
               (let [[[_ s l r]] (re-seq #"(...) = \((...), (...)\)" network-line)]
                 [s {:L l :R r}])))]))

(defn answer1 [input start]
  (let [[instructions network] (parse input)]
    (loop [inst (cycle instructions)
           state start
           steps 0]
      (if (= state "ZZZ")
        steps
        (let [state' (get-in network [state (first inst)])]
          (recur (rest inst) state' (inc steps)))))))

(is (= 14681 (answer1 day-8-input "AAA")))

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

(is (= 6 (answer2 input)))
(is (= 14321394058031 (answer2 day-8-input)))
