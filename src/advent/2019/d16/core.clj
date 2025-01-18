(ns advent.2019.d16.core
  (:require
   [clojure.string :as str]))

(def input (slurp "src/advent/2019/d16/input.txt"))
(def nums (->> (str/split (first (str/split-lines input)) #"") (mapv parse-long)))

(def len (count nums))
(def base-pattern [0 1 0 -1])

(defn pattern-fn [n]
  (->> base-pattern
       (map #(repeat n %))
       (apply concat)
       (repeat len)
       (apply concat)
       (take (inc len))
       (drop 1)
       vec))

(def patterns (mapv pattern-fn (mapv inc (range len))))

(defn apply-pattern [nums n]
  (->> (mapv * nums (get patterns n))
       (apply +)
       (#(rem % 10))
       abs))

(defn phase [nums]
  (mapv #(apply-pattern nums %) (range len)))

(println "ANS 1: "
         (->> (iterate phase nums)
              (drop 100)
              first
              (take 8)
              (apply str)))


;; PART 2
(def repeated-nums (->> nums (repeat 10000) (apply concat) vec))
(def message-offset (->> repeated-nums (take 7) (apply str) parse-long))

;; the offset is far off the middle of the number.
;; At the end of the number, the pattern is always 1 digits preceded by padding 0s
;; Each number is the sum of the previous numbers (counting from the end)
;; before:  x1            x2        x3      x4
;; pattern: 1111          0111      0011    0001
;; after:   x1+x2+x3+x4   x2+x3+x4  x3+x4   x4
;; digits are mod 10
;; 
;; The numbers before the off-set are not relevant for the calculations of
;; the digits after the offset

(def nums-after-offset (->> repeated-nums (drop message-offset) vec))

(defn phase-loop [nums-after-offset]
  (loop [nums nums-after-offset
         digs (list)
         s 0]
    (if (empty? nums)
      (vec digs)
      (let [last-dig (peek nums)
            s (+ s last-dig)
            digs (conj digs (mod s 10))]
        (recur (pop nums) digs s)))))

(println "ANS 2: "
         (->> (iterate phase-loop nums-after-offset)
              (drop 100)
              first
              (take 8)
              (apply str)))

