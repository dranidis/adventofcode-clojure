(ns advent.2020.d05.core
  (:require
   [clojure.string :as str]))

(def seats (str/split-lines (slurp "src/advent/2020/d05/input.txt")))

(defn- bin-search
  [chars h uch]
  (loop [chars chars
         l 0
         h h]
    (if (empty? chars)
      l
      (let [ch (nth chars 0)
            m (+ l (quot (- h l) 2))]
        (if (= uch ch)
          (recur (rest chars) (inc m) h)
          (recur (rest chars) l m))))))

(defn- seat-row
  [seat]
  (bin-search (take 7 seat) 127 \B))

(defn- seat-col
  [seat]
  (bin-search (drop 7 seat) 7 \R))

(defn seat-id
  [seat]
  (+ (* 8 (seat-row seat)) (seat-col seat)))

(def b-seats
  (->> seats
       (map seat-id)
       set))

(->> b-seats
     (apply max)
     println)

(->> (for [s b-seats
           :let [you (inc s)]
           :when (and (b-seats (inc you))
                      (b-seats (dec you))
                      (not (b-seats you)))]
       you)
     first
     println)

