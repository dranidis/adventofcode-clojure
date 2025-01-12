(ns advent.2019.d4.core
  (:require
   [advent.util :refer [coords-of-pred coords-of-symbol
                        parse-lines-with-numbers str->2D str->2D-num]]
   [clojure.string :as str]))

(def ddd
  (for [d1 (range 2 7)
        d2 (range d1 10)
        :when (>= (+ (* d1 10) d2) 27)
        d3 (range d2 10)
        d4 (range d3 10)
        d5 (range d4 10)
        d6 (range d5 10)
        :when (or (= d1 d2)
                  (= d2 d3)
                  (= d3 d4)
                  (= d4 d5)
                  (= d5 d6))]
    [d1 d2 d3 d4 d5 d6]))

(->> ddd
     count
     (println "ANS 1: "))

(defn groups [digits]
  (loop [digs (rest digits)
         cdig (first digits)
         cgroup [cdig]
         groups []]
    (if (empty? digs)
      (remove #(= 1 (count %)) (conj groups cgroup))
      (let [d (first digs)]
        (if (= d cdig)
          (recur (rest digs) cdig (conj cgroup d) groups)
          (recur (rest digs) d [d] (conj groups cgroup)))))))

(defn not-part-of-a-larger-group [digs]
  (>= (->> (groups digs)
           (map count)
           (filter #(= % 2))
           count)
      1))

(->> ddd
     (filter not-part-of-a-larger-group)
     count
     (println "ANS 2: "))
