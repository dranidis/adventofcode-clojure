(ns advent.2023.d4
  (:require [advent.2023.d4-input :refer [day-4-input]]
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
    (let [[[_ n l1 l2]] (re-seq #"Card.* (\d+): (.*)\|(.*)" line)
          card-nr (Integer/parseInt n)
          l1'  (mapv #(Integer/parseInt %) (s/split (s/trim l1) #"  *"))
          l2'  (mapv #(Integer/parseInt %) (s/split (s/trim l2) #"  *"))]
      {:id card-nr
       :winning (set l1')
       :have l2'})))

(defn matching-numbers [a-card]
  (let [c a-card]
    (for [n (:have c)
          :when ((:winning c) n)]
      n)))

(defn points-calc [nr]
  (if (= nr  0)
    0
    (if (= nr 1)
      1
      (* 2 (points-calc (dec nr))))))

(defn answer1 [input]
  (apply + (map #(points-calc (count (matching-numbers %))) (parse input))))

(is (= 13 (answer1 input)))


(comment

  (answer1 day-4-input)
;
  )



(comment
  (def cards (parse input))
  (def original-cards cards)
  (def new-cards cards)



  (def card (first new-cards))
  (def card-id (:id card))
  (def next-n (count (matching-numbers card)))
  (def cards-to-take (take next-n
                           (filter (fn [c]
                                     (> (:id c) card-id))
                                   original-cards)))
  (def new-cards (sort-by :id (apply conj (rest new-cards)
                                     (take next-n
                                           (filter (fn [c]
                                                     (> (:id c) card-id))
                                                   original-cards)))))

  new-cards


  ;
  )


  ;; all scores
(defn scores [cards]
  (mapv (fn [c] (count (matching-numbers c))) cards))

  ;; score of card with id
(defn score [scores id]
  (get scores (dec id)))

(defn add-all [map-num]
  (reduce-kv (fn [m k v]
               (+ m v))
             0
             map-num))

(defn increase [totals card-id next-n]
  (let [from (inc card-id)]
    (loop [n next-n
           from from
           totals totals]
      (if (= n 0)
        totals
        (recur (dec n) (inc from) (update totals from + (get totals card-id)))))))

(defn answer2 [cards]
  (add-all
   (let [all-scores (scores cards)
         number-of-cards (count all-scores)
         card-totals (reduce (fn [acc c]
                               (assoc acc (:id c) 1))
                             {}
                             cards)]
     (loop [card-id 1
            totals card-totals]
       (if (> card-id number-of-cards)
         totals
         (let [next-n (score all-scores card-id)
               totals' (increase totals card-id next-n)]
           (recur (inc card-id) totals')))))))

(answer2 (parse day-4-input))
