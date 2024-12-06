(ns advent.util
  (:require
   [clojure.string :as str]))

(defn parse-lines-with-numbers [input]
  (vec (for [line (str/split-lines input)]
         (mapv parse-long (re-seq #"\d+" line)))))

(comment
  (parse-lines-with-numbers "1 2 3
                             4 5 6
                             7 8 9")
  ;
  )

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c (str/trim line)]
                (str c))))))

(comment
  (str->2D "...#...
            ..#.#..
           .#...#.")
  ;
  )


(defn str->2D-num
  "Read a string containing new-lines into a 2 dimensional vector of numbers"
  [input]
  (mapv (fn [line] (mapv parse-long line))
        (str->2D input)))

(comment
  (str->2D-num "123
               456
               789")
  ;
  )

(defn transpose [m]
  (apply mapv vector m))

(comment
  (transpose [["1" "2" "3"]
              ["4" "5" "6"]
              ["7" "8" "9"]])
  ;
  )
