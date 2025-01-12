(ns advent.2019.d8.core
  (:require [clojure.string :as str]))

(def digs (mapv parse-long (str/split (slurp "src/advent/2019/d8/input.txt") #"")))

(def dims [25 6])
(def size (apply * dims))
(def layers (->> digs (partition size)))

(->> layers
     (apply min-key #(->> % (filter zero?) count))
     frequencies
     (#(* (get % 2) (get % 1)))
     (println "ANS 1:"))


(defn get-pixel [n]
  (loop [layers layers]
    (let [p (nth (first layers) n)]
      (if (= p 2)
        (recur (rest layers))
        (case p 1 "#" " ")))))

(println "ANS 2: READ THE MESSAGE")
(->> (range size)
     (mapv get-pixel)
     (partition (first dims))
     (map #(println (apply str %)))
     dorun)


