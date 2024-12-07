(ns advent.2024.d7.core
  (:require
   [clojure.string :as str]))

(comment
  (def input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")
 ;
  )

(def input (slurp "src/advent/2024/d7/input.txt"))

(defn parse
  [input]
  (for [line (str/split-lines input)]
    (mapv parse-long (re-seq #"\d+" line))))

(defn eqn?
  [ops equation]
  (let [test-value (first equation)
        eqn-operands (rest equation)]
    (some (fn [result] (= test-value result))
          (loop [operands (rest eqn-operands)
                 values [(first eqn-operands)]]
            (if (empty? operands)
              values
              (let [new-values (for [v values
                                     op ops]
                                 (op v (first operands)))]
                (recur (rest operands) new-values)))))))

(defn answer
  [ops]
  (apply +
         (mapv first
               (filter (partial eqn? ops)
                       (parse input)))))

(def answer-1 (answer [+ *]))

(defn ||
  [a b]
  (parse-long (str a b)))

(def answer-2 (answer [|| + *]))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

;; (-main)
