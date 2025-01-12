(ns advent.2019.d1.core
  (:require [clojure.string :as str]))

(defn fuel [f-fn]
  (->> (slurp "src/advent/2019/d1/input.txt")
       str/split-lines
       (map parse-long)
       (map f-fn)
       (apply +)))

(defn fuel-1 [l] (-> l
                     (quot 3)
                     (- 2)))

(println "ANS 1: " (fuel fuel-1))

(defn fuel-2 [l]
  (let [f1 (fuel-1 l)]
    (if (pos? f1)
      (+ f1 (fuel-2 f1))
      0)))

(println "ANS 1: " (fuel fuel-2))
