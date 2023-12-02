(ns advent.2023.d2
  (:require [advent.2023.d2-input :refer [day-2-input]]
            [clojure.string :as string]
            [clojure.test :refer [is]]))

;;  Performed some TDD this time

(def input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn- parse-set [set-str]
  (reduce (fn [acc num-color]
            (let [[n col] (string/split (string/trim num-color) #" ")]
              (assoc acc (keyword col) (read-string n))))
          {}
          (string/split set-str #",")))

(is (= {:blue 3
        :red 4} (parse-set " 3 blue, 4 red")))

(defn- parse-game [game-line]
  (let [game-parts (string/split game-line #"[:;]")
        id  (read-string (first (re-seq #"\d+" (get game-parts 0))))
        sets (map parse-set (rest game-parts))]
    {:id id :sets sets}))

(is (= {:id 1
        :sets [{:blue 3
                :red 4}
               {:red 1
                :green 2
                :blue 6}
               {:green 2}]}
       (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))

(defn parse-games [input]
  (for [game-line (string/split-lines input)]
    (parse-game game-line)))

(defn- game-max-of-color [game color-key]
  (apply max (map (fn [set]
                    (let [n (color-key set)]
                      (if (nil? n)
                        0
                        n))) (get game :sets))))

(is (= 6 (game-max-of-color {:id 1
                             :sets [{:blue 3
                                     :red 4}
                                    {:red 1
                                     :green 2
                                     :blue 6}
                                    {:green 2}]}
                            :blue)))

(defn possible-game? [game max-colors]
  (every? #(<= (game-max-of-color game %) (% max-colors))
          [:red :green :blue]))

(defn sum-of-possible-games [input]
  (apply + (map :id
                (filter (fn [g]
                          (possible-game? g {:red 12 :green 13 :blue 14}))
                        (parse-games input)))))

(is (= 8 (sum-of-possible-games input)))

;; answer 1
(is (= 2771 (sum-of-possible-games day-2-input)))

;; part 2

(defn game-max-colors [game]
  {:red (game-max-of-color game :red)
   :green (game-max-of-color game :green)
   :blue (game-max-of-color game :blue)})

(is (= {:red 4 :green 2 :blue 6}
       (game-max-colors {:id 1
                         :sets [{:blue 3
                                 :red 4}
                                {:red 1
                                 :green 2
                                 :blue 6}
                                {:green 2}]})))

(defn power [set]
  (* (:red set) (:green set) (:blue set)))

(defn total-power [input]
  (apply + (map (fn [game]
                  (power (game-max-colors game)))
                (parse-games input))))

(is (= 2286 (total-power input)))

;; answer 2
(is (= 70924 (total-power day-2-input)))

