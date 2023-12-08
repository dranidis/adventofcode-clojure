(ns advent.2023.d7
  (:require [advent.2023.d7-input :refer [day-7-input]]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
")

(defn- parse-hand [hand-str]
  (str/split hand-str #""))


(defn- parse [input]
  (for [line (str/split-lines input)]
    (let [[hand n] (str/split line #"  *")]
      [(str/split hand #"") (read-string n)])))

(def cards ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(defn group-cards [hand]
  (sort >
        (map count
             (vals (group-by identity hand)))))

(is (= (list 2 1 1 1) (group-cards (parse-hand "12334"))))

(defn all-groupings [hand]
  (map (fn [r]
         (group-cards (map (fn [c] (if (= c "J") r c))
                           hand)))
       cards))

;; changed for part 2
(defn- hand-type [hand]
  (let [group-cards (group-cards hand)
        all-card-groupings (all-groupings hand)]
    (cond
      (or (apply = hand)
          (some (fn [g] (= 5 (first g))) all-card-groupings))
      :1-five

      (or (= 4 (first group-cards))
          (some (fn [g] (= 4 (first g))) all-card-groupings))
      :2-four

      (or (= (list 3 2) group-cards)
          (some (fn [g] (= (list 3 2) g)) all-card-groupings))
      :3-fh

      (or (= 3 (first group-cards))
          (some (fn [g] (= 3 (first g))) all-card-groupings))
      :4-three

      (or (= (list 2 2 1) group-cards)
          (some (fn [g] (= (list 2 2 1) g)) all-card-groupings))
      :5-2p

      (or (= (list 2 1 1 1) group-cards)
          (some (fn [g] (= (list 2 1 1 1) g)) all-card-groupings))
      :6-1p

      :else
      :7-hc)))

(is (= :1-five (hand-type (parse-hand "AAAAA"))))
(is (= :2-four (hand-type (parse-hand "AA8AA"))))
(is (= :3-fh (hand-type (parse-hand "23332"))))
(is (= :4-three (hand-type (parse-hand "TTT98"))))
(is (= :5-2p (hand-type (parse-hand "23432"))))
(is (= :6-1p (hand-type (parse-hand "A23A4"))))
(is (= :7-hc (hand-type (parse-hand "23456"))))

(is (= :2-four (hand-type (parse-hand "QJJQ2"))))
(is (= :1-five (hand-type (parse-hand "AAJAA"))))
(is (= :3-fh (hand-type (parse-hand "23J32"))))

;; changed for part 2
(def card-rank {"A" 0 "K" 1 "Q" 2  "T" 4
                "9" 5 "8" 6 "7" 7 "6" 8 "5" 9 "4" 10 "3" 11 "2" 12 "J" 13})

(defn- to-numbers [hand1]
  (mapv card-rank hand1))

(is (= [11 12 4 11 1] (to-numbers ["3" "2" "T" "3" "K"])))

(defn compare-hands [[hand1 _] [hand2 _]]
  (let [f1 (Integer/parseInt (str (first (name (hand-type hand1)))))
        f2 (Integer/parseInt (str (first (name (hand-type hand2)))))
        n1 (to-numbers hand1)
        n2 (to-numbers hand2)]
    (cond (> f1 f2)
          -1
          (< f1 f2)
          1
          :else
          (compare n2 n1))))

(defn- process [input]
  (let [hands (map-indexed (fn [idx itm] [(inc idx) itm])
                           (sort compare-hands (parse input)))]
    (map (fn [[r [_ b]]]
           [r b])
         hands)))

(defn total-winnings-of-set [input]
  (apply  +
          (map (fn [[rank bid]]
                 (* bid rank)) (process input))))

(comment
;;   part 1
  (is (= 6440 (total-winnings-of-set input)))
  ;
  )

(is (= 5905 (total-winnings-of-set input)))

