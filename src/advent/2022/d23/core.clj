(ns advent.2022.d23.core
  (:require
   [advent.util :refer [coords-of-symbol str->2D]]))

(def grid (str->2D (slurp "src/advent/2022/d23/input.txt")))

(def elves (coords-of-symbol grid "#"))

(def ds (vec (for [r [-1 0 1]
                   c [-1 0 1]
                   :when (not (#{[0 0]} [r c]))]
               [r c])))

(def dirs [[-1 0]
           [1 0]
           [0 -1]
           [0 1]])

(defn others-around [s-elves e]
  (for [d ds
        :let [o (mapv + e d)]
        :when (s-elves o)]
    o))

(defn moving? [s-elves]
  (fn [e] (seq (others-around s-elves e))))


(defn rule-at-index [rule-i]
  (nth (mapv vec [(for [c [-1 0 1]] [-1 c])
                  (for [c [-1 0 1]] [1 c])
                  (for [r [-1 0 1]] [r -1])
                  (for [r [-1 0 1]] [r 1])])
       rule-i))

(def r-at-i (mapv rule-at-index (range 4)))

(defn proposes [s-elves rule-i e]
  (if ((moving? s-elves) e)
    (loop [rule-i rule-i
           cnt 0]
      (if (> cnt 4)
        nil
        (let [rule-at-i (nth r-at-i rule-i)
              others (for [d rule-at-i
                           :let [o (mapv + e d)]
                           :when (s-elves o)]
                       o)]
          (if (empty? others)
            (mapv + e (nth dirs rule-i))
            (recur (mod (inc rule-i) 4) (inc cnt))))))
    nil))

(defn proposed-positions [elves rule-i]
  (let [s-elves (set elves)]
    (->> elves
         (mapv (fn [e] (proposes s-elves rule-i e))))))

(defn after [times]
  (loop [elves elves
         rule-i 0
         round 0]
    (println "Round " round)
    (if (= times round)
      elves
      (if (not-any? (moving? (set elves)) elves)
        round
        (let [pp (proposed-positions elves rule-i)
              elves (mapv (fn [e p]
                            (if (and (some? p)
                                     (= 1 (count (take 2 (filter #(= % p) pp)))))
                              p
                              e))
                          elves pp)]
          (recur elves (mod (inc rule-i) 4) (inc round)))))))

(def after-10 (after 10))
(def minr (apply min (map first after-10)))
(def maxr (apply max (map first after-10)))
(def minc (apply min (map second after-10)))
(def maxc (apply max (map second after-10)))

(println "ANS 1: " (- (* (inc (- maxr minr)) (inc (- maxc minc))) (count elves)))

(println "ANS 2: " (inc (after nil)))

