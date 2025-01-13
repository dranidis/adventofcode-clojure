(ns advent.2019.d10.core
  (:require
   [advent.util :refer [xy-of-symbol
                        str->2D]]))
(def grid (str->2D (slurp "src/advent/2019/d10/input.txt")))

(def Xs (xy-of-symbol grid "#"))

(defn slope [dy dx]
  (if (zero? dx)
    (if (pos? dy)
      Double/POSITIVE_INFINITY
      Double/NEGATIVE_INFINITY)
    (float (/ dy dx))))

(def D (into {} (vec (for [[x1 y1 :as pos1] Xs
                           [x2 y2 :as pos2] Xs
                           :when (not= pos1 pos2)
                           :let [dx (- x2 x1)
                                 dy (- y2 y1)
                                 s (slope dy dx)
                                 d (+ (* dx dx) (* dy dy))]]
                       [[pos1 pos2] [dx dy s d]]))))


(defn- others-in-line-of-sight [[px py :as p] [x y :as co]]
  (let [[dx dy s d] (D [co p])]
    [s d dx (->> (for [[ox oy :as o] Xs
                       :when (and (not= [px py] [ox oy])
                                  (not= [x y] [ox oy]))
                       :let [[odx ody oslope od] (D [o p])]
                       :when (and (= s oslope)
                                  (> (+ (* dx odx) (* dy ody)) 0))
                       :when (and (not (zero? od))
                                  (< od d))]
                   [[ox oy] oslope od odx]))]))

(defn detected-from? [pos x] (->> (nth (others-in-line-of-sight pos x) 3) count zero?))

(def best-point-detected (->> (for [pos Xs]
                                [pos (->> (for [x Xs
                                                :when (not= x pos)
                                                :when (detected-from? pos x)]
                                            x)
                                          count)])
                              (apply max-key second)))

(println "ANS 1: " (second best-point-detected))

(def s (first best-point-detected))

;; grouped and sorted by direction and sorted by distance
(def sss (->> (for [p Xs
                    :when (not= p s)
                    :let [[dx dy s d] (D [s p])
                          dir (if (> (+ dx (* s dy)) 0) 0 1)]]
                [dx dy [dir s] d])
              (group-by (fn [[dx dy dir-s d]] dir-s))
              (into [])
              sort
              (map (fn [[dir-s y]] (sort-by (fn [[dx dy s d]] d) y)))))

(println "ANS 2: " (loop [sss sss
                          cnt 1]
                     (if (= cnt 200)
                       (let [pos (ffirst sss)
                             [x y] (mapv + s pos)]
                         (+ (* 100 x) y))
                       (let [r (vec (rest (first sss)))]
                         (if (or (nil? r) (empty r))
                           (recur (vec (rest sss)) (inc cnt))
                           (recur (conj (vec (rest sss)) r) (inc cnt)))))))


