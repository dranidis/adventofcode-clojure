(ns advent.2020.d13.core
  (:require
   [advent.util :refer [chinese-remainder-theorem]]
   [clojure.string :as str]))

(def example? false)

(def example "939
7,13,x,x,59,x,31,19")

(def input (if example? example (slurp "src/advent/2020/d13/input.txt")))
(def l1 (first (str/split-lines input)))
(def l2 (second (str/split-lines input)))

(def earliest (parse-long l1))
(def bus-ids (->> (str/split l2 #",")
                  (remove #(= % "x"))
                  (map parse-long)))

;; Part 1
(println (ffirst (take 1 (for [after (range)
                               :let [t (+ earliest after)]
                               bus bus-ids
                               :when (zero? (mod t bus))]
                           [(* bus after)]))))

;; Part 2
(def bus-x-ids (->> (str/split l2 #",")
                    (map parse-long)
                    (map-indexed (fn [idx itm] [idx itm]))
                    (remove #(nil? (second %)))))

(def r-mods (map (fn [[x y]] [(mod (- y x) y) y]) bus-x-ids))

(println (chinese-remainder-theorem r-mods))
