(ns advent.2024.d15.core
  (:require
   [advent.2024.d15.core2 :refer [answer-2]]
   [advent.util :refer [str->2D]]
   [clojure.string :as str]))

(def example? false)

(def example
  "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(def input (if example? example (slurp "src/advent/2024/d15/input.txt")))

;; SECTIONS
(def map-section (first (str/split input #"\n\n")))
(def move-section (second (str/split input #"\n\n")))

;; GRID
(def grid (str->2D map-section))
(def rows (count grid))
(def cols (count (first grid)))

;; MOVES

(def moves (str/split (str/join (str/split-lines move-section)) #""))
;; (count moves)

(def robot (first (for [r (range rows)
                        c (range cols)
                        :when (= (get-in grid [r c]) "@")]
                    [r c])))

(def boxes (set (for [r (range rows)
                      c (range cols)
                      :when (= (get-in grid [r c]) "O")]
                  [r c])))

(def walls (set (for [r (range rows)
                      c (range cols)
                      :when (= (get-in grid [r c]) "#")]
                  [r c])))

(defn- move-box [move box boxes]
  (let [[nr nc] box
        new-box (case move
                  "v" [(inc nr) nc]
                  "^" [(dec nr) nc]
                  ">" [nr (inc nc)]
                  "<" [nr (dec nc)])]
    (if (walls new-box)
      [boxes false]
      (if (boxes new-box)
        (let [[new-boxes moved?] (move-box move new-box boxes)]
          (if (not moved?)
            [boxes false]
            [(conj (disj new-boxes box) new-box) true]))
        [(conj (disj boxes box) new-box) true]))))

(def new-boxes
  (loop [moves moves
         robot robot
         boxes boxes]

    (if (empty? moves)
      boxes
      (let [move (first moves)
            [r c] robot
            new-robot (case move
                        "v" [(inc r) c]
                        "^" [(dec r) c]
                        ">" [r (inc c)]
                        "<" [r (dec c)])]
        (if (walls new-robot)
          (recur (rest moves) robot boxes)
          (if (boxes new-robot)
            (let [[new-boxes moved?] (move-box move new-robot boxes)
                  new-rc (if moved? new-robot robot)]
              (recur (rest moves) new-rc new-boxes))
            (recur (rest moves) new-robot boxes)))))))

(def answer-1 (apply + (map (fn [[r c]] (+ (* 100 r) c)) new-boxes)))

(defn- -main [& _]
  (println "2024, Day 15, Part 1:" answer-1)
  (println "2024, Day 15, Part 2:" answer-2))
