(ns advent.2019.d6.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def orbits (->> (slurp "src/advent/2019/d6/input.txt")
                 (str/split-lines)
                 (mapv (fn [l] (-> (str/split l #"\)")
                                   reverse
                                   vec)))
                 (into {})))

(def objects (->> orbits (into []) flatten distinct))

(defn orbiting-objects [orbits m]
  (let [o (orbits m)]
    (if (nil? o)
      []
      (let [mm (orbiting-objects orbits o)]
        (apply conj [o] mm)))))

(println "ANS 1: "
         (->> objects
              (mapv #(count (orbiting-objects orbits %)))
              (apply +)))

(def you (orbiting-objects orbits "YOU"))
(def san (orbiting-objects orbits "SAN"))
(def inter (set/intersection (set you) (set san)))

(println "ANS 2: " (+ (apply min (mapv #(.indexOf you %) inter))
                      (apply min (mapv #(.indexOf san %) inter))))


