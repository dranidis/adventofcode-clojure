(ns advent.2021.d7.core)

(comment
  (def input "16,1,2,0,4,2,7,1,2,14")
 ;
  )

(def input (slurp "src/advent/2021/d7/input.txt"))

(defn parse-line
  [input]
  (mapv parse-long (re-seq #"\d+" input)))

(def parsed (parse-line input))
(def max-pos (apply max parsed))
(def min-pos (apply min parsed))

(def answer-1 (apply min (for [i (range min-pos (inc max-pos))]
                           (apply + (map #(abs (- % i)) parsed)))))

(defn consume
  [i crab]
  (let [d (abs (- crab i))]
    (/ (* d (inc d)) 2)))

(def answer-2 (apply min (for [i (range min-pos (inc max-pos))]
                           (apply + (map (partial consume i) parsed)))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)

