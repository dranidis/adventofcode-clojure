(ns advent.2023.d4
  (:require [advent.2023.d4-input :refer [day-4-input]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [clojure.test :refer [is]]))

(def input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse [input]
  (for [line (s/split-lines input)]
    (let [[[_ id l1 l2]] (re-seq #"Card.* (\d+): (.*)\|(.*)" line)
          l1' (read-string (str "[" l1 "]"))
          l2' (read-string (str "[" l2 "]"))]
      {:id (parse-long id)
       :winning (set l1')
       :have (set l2')})))

(defn matching-numbers [a-card]
  (set/intersection (:have a-card) (:winning a-card)))

(defn points-calc [nr]
  (if (zero? nr) 0 (math/pow 2 (dec nr))))

(defn answer1 [input]
  (apply + (map #(points-calc (count (matching-numbers %)))
                (parse input))))

(is (== 21158 (answer1 day-4-input)))


(defn scores [cards]
  (mapv (fn [c] (count (matching-numbers c))) cards))

(defn increase [totals card-id next-n]
  (let [from (inc card-id)]
    (loop [n next-n
           from from
           totals totals]
      (if (= n 0)
        totals
        (recur (dec n) (inc from) (update totals from + (get totals card-id)))))))

(defn answer2 [cards]
  (apply +
         (vals (let [all-scores (scores cards)
                     number-of-cards (count cards)
                     initial-totals (zipmap (map :id cards)
                                            (repeat 1))]
                 (loop [card-id 1
                        totals initial-totals]
                   (if (> card-id number-of-cards)
                     totals
                     (let [next-n (get all-scores (dec card-id))
                           totals' (increase totals card-id next-n)]
                       (recur (inc card-id) totals'))))))))


(is (= 6050769 (answer2 (parse day-4-input))))

