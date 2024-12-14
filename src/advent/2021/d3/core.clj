(ns advent.2021.d3.core
  (:require
   [advent.util :refer [str->2D-num]]
   [clojure.string :as str]))

;; (comment
(def input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")
 ;
  ;; )

(def input (slurp "src/advent/2021/d3/input.txt"))

(def bins (str->2D-num input))
(def len (count (first bins)))

(defn more-1s
  [bins n]
  (if (>= (apply + (mapv (fn [b] (get b n)) bins)) (/ (count bins) 2))
    1
    0))

(def gamma-bin (map (partial more-1s bins) (range len)))

(def alpha-bin (mapv (fn [b] (if (= b 1) 0 1)) gamma-bin))

(def answer-1 (* (Long/parseLong (str/join "" gamma-bin) 2)
                 (Long/parseLong (str/join "" alpha-bin) 2)))

(defn oxyg-rating-bin-fn
  [neg?]
  (loop [bins bins
         n 0]
    (if (= n len)
      nil
      (let [ones? (more-1s bins n)
            new-bins (vec
                      (filter (fn [b] (if (= ones? 1)
                                        (= (if neg? 1 0) (get b n))
                                        (= (if neg? 0 1) (get b n))))
                              bins))]
        (if (= 1 (count new-bins))
          (first new-bins)
          (recur new-bins (inc n)))))))

(def oxyg-rating-bin
  (oxyg-rating-bin-fn false))

(def co2-rating-bin
  (oxyg-rating-bin-fn true))

(def answer-2 (* (Long/parseLong (str/join "" oxyg-rating-bin) 2)
                 (Long/parseLong (str/join "" co2-rating-bin) 2)))


(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)
