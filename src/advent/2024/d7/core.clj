(ns advent.2024.d7.core
  (:require
   [advent.util :refer [str->2D]]))

(comment
  (def input "")
 ;
  )

(def input (slurp "src/advent/2024/d7/input.txt"))

(def the-map (str->2D input))



(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

