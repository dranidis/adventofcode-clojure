(ns advent.2020.d22.core
  (:require
   [clojure.string :as str]))

(defn parse-player [p]
  (->> p str/split-lines
       (drop 1)
       (mapv parse-long)))

(def sections (str/split (slurp "src/advent/2020/d22/input.txt") #"\n\n"))
(def player-1 (->> sections
                   first
                   parse-player))
(def player-2 (->> sections
                   second
                   parse-player))

(defn- add-cards-to [p c1 c2]
  (-> p (conj c1) (conj c2)))

(defn game
  [player-1 player-2]
  (->> (loop [p1 player-1
              p2 player-2
              rnd 0]
         (if (or (empty? p1) (empty? p2))
           [p1 p2]
           (let [c1 (first p1)
                 c2 (first p2)
                 [p1 p2] (if (> c1 c2)
                           [(add-cards-to (vec (rest p1)) c1 c2) (vec (rest p2))]
                           [(vec (rest p1)) (add-cards-to (vec (rest p2)) c2 c1)])]
             (recur p1 p2 (inc rnd)))))))

(defn score-game [g]
  (->> (first (remove empty? g))
       reverse
       (map-indexed vector)
       (map (fn [[i v]] (* (inc i) v)))
       (apply +)))

(->> (game player-1 player-2)
     score-game
     (println "ANS 1: "))

;; PART 2

(defn game-rec
  [player-1 player-2 depth]
  (->> (loop [p1 player-1
              p2 player-2
              D #{}
              rnd 0]
         (if (or (empty? p1) (empty? p2))
           [p1 p2]
           (let [card1 (first p1)
                 card2 (first p2)
                 r1 (vec (rest p1))
                 r2 (vec (rest p2))
                 [p1 p2] (let [cnt1 (count r1)
                               cnt2 (count r2)]
                           (if (and (>= cnt1 card1) (>= cnt2 card2))
                             (let [pl1 (vec (take card1 r1))
                                   pl2 (vec (take card2 r2))
                                   [_ pl2] (game-rec pl1 pl2 (inc depth))]
                               (if (empty? pl2) ;; pl1 won
                                 [(add-cards-to r1 card1 card2) r2]
                                 [r1 (add-cards-to r2 card2 card1)]))
                             (if (> card1 card2)
                               [(add-cards-to r1 card1 card2) r2]
                               [r1 (add-cards-to r2 card2 card1)])))]
             (if (D [p1 p2])
               [p1 []]
               (recur p1 p2 (conj D [p1 p2]) (inc rnd))))))))

(->> (game-rec player-1 player-2 0)
     score-game
     (println "ANS 2: "))

