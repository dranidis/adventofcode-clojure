(ns advent.2020.d06.core
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(def groups (str/split (slurp "src/advent/2020/d06/input.txt") #"\n\n"))

(->> groups
     (map (fn [g]
            (->> g
                 set
                 (remove (fn [c] (= c \newline)))
                 count)))
     (apply +)
     println)

(->> groups
     (map (fn [g]
            (->> g
                 (str/split-lines)
                 (map set)
                 (apply set/intersection)
                 count)))
     (apply +)
     println)