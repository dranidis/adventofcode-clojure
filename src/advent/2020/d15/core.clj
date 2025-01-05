(ns advent.2020.d15.core
  (:require
   [clojure.string :as str]))

(def input "13,16,0,12,15,1")

(def numbers (->> (str/split input #",") (mapv (partial parse-long))))

(def Dinit (->> (map-indexed vector numbers)
                (mapv (fn [[i v]] [v (inc i)]))
                pop
                (into {})))

(defn play
  [[last-n D current-round]]
  (if (contains? D last-n)
    (let [n (- current-round (get D last-n))]
      [n (assoc D last-n current-round) (inc current-round)])
    [0 (assoc D last-n current-round) (inc current-round)]))

(defn answer [at]
  (->> (iterate play [(last numbers) Dinit (count numbers)])
       (take (- at (dec (count numbers))))
       last
       first
       println))


(answer 2020)
(answer 30000000)

