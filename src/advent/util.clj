(ns advent.util
  (:require
   [clojure.string :as str]))

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
                (str c))))))

(def input "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
")


(defn str->2D-num
  "Read a string containing new-lines into a 2 dimensional vector of numbers"
  [input]
  (mapv (fn [line] (mapv parse-long line))
        (str->2D input)))