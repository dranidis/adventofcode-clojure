(ns advent.2021.d25.core
  (:require
   [advent.util :refer [coords-of-symbol str->2D]]
   [clojure.set :as set]))

(def grid (str->2D (slurp "src/advent/2021/d25/input.txt")))
(def rows (count grid))
(def cols (count (first grid)))

(def Es (coords-of-symbol grid ">"))
(def Ds (coords-of-symbol grid "v"))

(defn moving [Ds-Es n rows-cols]
  (let [sets (apply set/union (mapv set Ds-Es))]
    (mapv (fn [pos]
            (let [nx (mod (inc (nth pos n)) (nth rows-cols n))
                  npos (assoc pos n nx)]
              (if-not (sets npos)
                npos
                pos)))
          (nth Ds-Es n))))

(println "ANS 1"
         (loop [Es Es
                Ds Ds
                stable? false
                t 0]
           (if stable?
             t
             (let [mEs (moving [Ds Es] 1 [rows cols])
                   mDs (moving [Ds mEs] 0 [rows cols])
                   stable? (and  (= Es mEs) (= Ds mDs))]
               (recur mEs mDs stable? (inc t))))))

