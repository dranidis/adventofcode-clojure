(ns advent.2023.d2.core
  (:require [clojure.string :as str]))

(def example (slurp "src/advent/2023/d2/example.txt"))
(def input (slurp "src/advent/2023/d2/input.txt"))

(defn parse-set [set-str]
  (reduce (fn [acc num-color]
            (let [[n col] (str/split (str/trim num-color) #" ")]
              (assoc acc (keyword col) (parse-long n))))
          {}
          (str/split set-str #",")))

(defn parse-game [game-line]
  (let [game-parts (str/split game-line #"[:;]")
        id  (parse-long (re-find #"\d+" (first game-parts)))
        sets (map parse-set (rest game-parts))]
    {:id id :sets sets}))

(defn parse-games [input]
  (for [game-line (str/split-lines input)]
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

(defn -main [& _]
  (println "Day 2, Part 1:" (sum-of-possible-games input))
  (println "Day 2, Part 2:" (total-power input)))