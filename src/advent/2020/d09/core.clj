(ns advent.2020.d09.core
  (:require [clojure.string :as str]))

(def numbers
  (->> (slurp "src/advent/2020/d09/input.txt")
       (str/split-lines)
       (map parse-long)
       vec))

(def invalid-num
  (->> (partition 26 1 numbers)
       (filter (fn [a-partition]
                 (empty? (for [n1 a-partition
                               n2 a-partition
                               :when (not= n1 n2)
                               :let [n (last a-partition)]
                               :when (= n (+ n1 n2))]
                           [n1 n2 n]))))
       first
       last))

(println invalid-num)

(defn sum-max-and-min [coll]
  (+ (apply max coll) (apply min coll)))

(println
 (loop [drop-n 0
        take-n 0
        sum 0
        nums []]
   (if (= drop-n (count numbers))
     nil
     (if (= sum invalid-num)
       (sum-max-and-min nums)
       (if (> sum invalid-num)
         (recur (inc drop-n) 0 0 [])
         (let [n (nth numbers (+ drop-n take-n))
               sum (+ n sum)
               nums (conj nums n)]
           (recur drop-n (inc take-n) sum nums)))))))

