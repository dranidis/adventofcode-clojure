(ns advent.2021.d1.core
  (:require
   [clojure.string :as str]))

(comment
  (def input "199
200
208
210
200
207
240
269
260
263
")
 ;
  )

(def input (slurp "src/advent/2021/d1/input.txt"))

(def answer-1 (count (filter true? (map (fn [[a b]] (< a b))
                                        (partition 2 1
                                                   (mapv parse-long (str/split-lines input)))))))

(def answer-2 (count (filter true? (map (fn [[a b]] (< a b))
                                        (partition 2 1
                                                   (map (fn [l] (apply + l)) (partition 3 1 (mapv parse-long (str/split-lines input)))))))))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)

