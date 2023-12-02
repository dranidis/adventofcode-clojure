(ns advent.2023.d2
  (:require [clojure.string :as s]))

(defn parse-set [set-str]
  (reduce (fn [acc num-color]
            (let [[n col] (s/split (s/trim num-color) #" ")]
              (assoc acc (keyword col) (read-string n))))
          {}
          (s/split set-str #",")))

(defn parse-game [game-line]
  (let [game-parts (s/split game-line #"[:;]")
        id  (read-string (first (re-seq #"\d+" (first game-parts))))
        sets (map parse-set (rest game-parts))]
    {:id id :sets sets}))

(defn parse-games [input]
  (for [game-line (s/split-lines input)]
    (parse-game game-line)))

(defn game-max-of-color [game color-key]
  (apply max (map #(get % color-key 0) (:sets game))))

(defn possible-game? [game max-colors]
  (every? #(<= (game-max-of-color game %) (% max-colors))
          [:red :green :blue]))

(defn sum-of-possible-games [input]
  (apply + (map :id
                (filter (fn [g]
                          (possible-game? g {:red 12 :green 13 :blue 14}))
                        (parse-games input)))))

;; answer 1
;; (prn (sum-of-possible-games day-2-input))

;; part 2

(defn game-max-colors [game]
  {:red (game-max-of-color game :red)
   :green (game-max-of-color game :green)
   :blue (game-max-of-color game :blue)})

(defn power [set]
  (* (:red set) (:green set) (:blue set)))

(defn total-power [input]
  (apply + (map (fn [game]
                  (power (game-max-colors game)))
                (parse-games input))))

;; answer 2
;; (prn (total-power day-2-input))

