(ns advent.2024.d15.core
  (:require
   [advent.util :refer [draw-grid draw-grid-from-points str->2D]]
   [clojure.string :as str]))

(def example? true)

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

(draw-grid grid)

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


(defn- move-box [move robot new-robot boxes]
  (let [[nr nc] new-robot
        new-box (case move
                  "v" [(inc nr) nc]
                  "^" [(dec nr) nc]
                  ">" [nr (inc nc)]
                  "<" [nr (dec nc)])]
    (if (walls new-box)
      [boxes robot]
      (if (boxes new-box)
        (let [[new-boxes new-robot] (move-box move new-robot new-box boxes)]
          [new-boxes new-robot])
        [(conj (disj boxes new-robot) new-box) new-robot]))))

(def new-boxes
  (loop [moves moves
         robot robot
         boxes boxes]

    (draw-grid-from-points rows cols boxes)

    (if (empty? moves)
      boxes
      (let [move (first moves)
            _ (println move)
            [r c] robot
            [new-r new-c] (case move
                            "v" [(inc r) c]
                            "^" [(dec r) c]
                            ">" [r (inc c)]
                            "<" [r (dec c)])]
        (if (walls [new-r new-c])
          (recur (rest moves) robot boxes)
          (if (boxes [new-r new-c])
            (let [[new-boxes new-robot] (move-box move robot [new-r new-c] boxes)
                  new-robot (if (= new-robot robot) robot [new-r new-c])]
              (recur (rest moves) new-robot new-boxes))
            (recur (rest moves) [new-r new-c] boxes)))))))

(apply + (map (fn [[r c]] (+ (* 100 r) c)) new-boxes))

;; 1346831 too low

;; (defn- -main [& _]
;;   (println "Day XX, Part 1:" answer-1)
;;   (println "Day XX, Part 2:" answer-2))

;; (-main)
