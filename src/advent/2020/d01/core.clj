(ns advent.2020.d01.core
  (:require
   [clojure.string :as str]))

(def n (->> (slurp "src/advent/2020/d01/input.txt")
            str/split-lines
            (mapv parse-long)))

(->> (for [x1 n
           x2 n
           :when (= 2020 (+ x1 x2))]
       (* x1 x2))
     first println)

(->> (for [x1 n
           x2 n
           x3 n
           :when (= 2020 (+ x1 x2 x3))]
       (* x1 x2 x3))
     first println)

