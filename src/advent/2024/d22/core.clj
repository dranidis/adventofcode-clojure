(ns advent.2024.d22.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(def example? false)

(def example "1
10
100
2024")

(def input (if example? example (slurp "src/advent/2024/d22/input.txt")))
(def numbers (mapv parse-long (str/split-lines input)))

(defn next-secret
  [secret]
  (letfn [(mix-and-prune [n v] (mod (bit-xor n v) 16777216))]
    (-> secret
        (#(mix-and-prune % (* % 64)))
        (#(mix-and-prune % (quot % 32)))
        (#(mix-and-prune % (* % 2048))))))

(defn secret-repeat
  [times number]
  (first (drop times (iterate next-secret number))))

(is (= 8685429 (secret-repeat 2000 1)))

(def answer-1 (apply + (map (partial secret-repeat 2000) numbers)))

(defn secret-sequences
  [times n]
  (loop [number n
         prev-digit nil
         change-sequence []
         counter {}
         t 0]
    (if (= t times)
      counter
      (let [last-dig (mod number 10)
            change ((fnil #(- last-dig %) 0) prev-digit)
            change-sequence (conj change-sequence change)
            change-sequence (if (> (count change-sequence) 4)
                              (vec (drop 1 change-sequence))
                              change-sequence)
            counter (if (get counter change-sequence)
                      counter
                      (assoc counter change-sequence last-dig))]
        (recur (next-secret number) last-dig change-sequence  counter (inc t))))))

(def answer-2
  (->> (if example? [1 2 3 2024] numbers)
       (map (partial secret-sequences 2000))
       (apply merge-with +)
       vals
       (apply max)))

(defn- -main [& _]
  (println "Day XX, Part 1:" answer-1)
  (println "Day XX, Part 2:" answer-2))
