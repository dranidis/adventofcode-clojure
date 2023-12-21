(ns advent.2023.d21.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
                (str c))))))



(defn answer [input times]
  (let [pi (str->2D input)
        rows (count pi)
        cols (count (first pi))
        [start-set rocks-set]
        (letfn [(scan [ch]
                  (set (for [r (range rows)
                             c (range cols)
                             :let [tile (get-in pi [r c])]
                             :when (= tile ch)]
                         [r c])))]
          [(scan "S") (scan "#")])]
    (loop [poss start-set
           t 1]
            ;;  (println poss)
      (if (> t times)
        (count poss)
        (let [new-poss
              (set (for [[r c] poss
                         [dir-r dir-c] [[0 1] [0 -1] [1 0] [-1 0]]
                         :let [new-r (+ r dir-r)
                               new-c (+ c dir-c)]
                         :when (and (<= 0 new-r (dec rows))
                                    (<= 0 new-c (dec cols))
                                    (not (rocks-set [new-r new-c])))]
                     [new-r new-c]))]
          (recur new-poss (inc t)))))))

(defn -main [& _]
  (let [input
      ;;   input
        (slurp "src/advent/2023/d21/input.txt")
        ;
        ]
    (time (println "Day 1, Part 1:" (answer input 64)))
;;   (println "Day 1, Part 2:" (total-2 input))
    ))

