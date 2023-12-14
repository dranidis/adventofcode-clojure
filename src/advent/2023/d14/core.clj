(ns advent.2023.d14.core
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")


(defn transpose [m]
  (apply mapv vector m))

;; rocsk are transposed
;; the columns are in rows 
;; and the rocks need now to move to the left
(defn parse [input]
  (transpose (vec (for [line (str/split-lines input)]
                    (str/split line #"")))))

(defn rocks-at-col [column ch]
  (vec (for [x (range (count column))
             :let [m (get column x)]
             :when (= m ch)]
         x)))

(defn tilt-a-rock [i O-index O-rocks hashes]
  (let [hashes (conj hashes 999999)
        all-rocks (vec (sort (concat O-rocks hashes)))
        below-O-rocks (filter (fn [i] (< i O-index)) all-rocks)
        on-hash-rock (first (first
                             (filter (fn [[p1 p2]]
                                       (< p1 O-index p2)) (partition 2 hashes))))]
    (if (empty? below-O-rocks)
      (assoc O-rocks i (if (nil? on-hash-rock) 0 on-hash-rock))
      (assoc O-rocks i (if (nil? on-hash-rock)
                         (inc (last below-O-rocks))
                         (inc (max (last below-O-rocks) on-hash-rock)))))))

(is (= [0 6 7] (tilt-a-rock 2 9 [0 6 9] [5])))

(defn tilt-numbers [[Os hashes]]
  [(reduce (fn [O-rocks [i O-index]]
             (let [t (tilt-a-rock i O-index O-rocks hashes)]
               t))
           Os
           (map-indexed vector Os)) hashes])

(defn tilt-column [column]
  (let [Os (rocks-at-col column "O")
        hashes (rocks-at-col column "#")]
    (tilt-numbers [Os hashes])))

(is (= [[0 1 2 3] [8 9]] (tilt-column (first (parse input)))))
(is (= [[0 6 7] [5]] (tilt-column (nth (parse input) 2))))
(is (= [[2 6] [1 5]] (tilt-column (last (parse input)))))

(defn total-load [reflector len]
  (apply + (flatten (map
                     (fn [[c _]]
                       (map (fn [n] (- len n)) c))
                     reflector))))

(defn answer-1 [input]
  (let [pi (parse input)
        len (count (first pi))]
    (total-load (map tilt-column pi) len)))

(is (= 136 (answer-1 input)))
;; (is (= 113424 (answer-1 (slurp "src/advent/2023/d14/input.txt"))))


;;  part 2

(defn rotate-numbers
  "Rotate the indices of Os and #s CCW. Not sure why the adjustment with sub? is needed, but..."
  [dim sub? reflector]
  (->> reflector
       (map-indexed vector)
       (map (fn [[col [Os hashes]]]
              [(apply concat [(map (fn [o] [o (if sub? (- dim col) col)]) Os)])
               (apply concat [(map (fn [o] [o (if sub? (- dim col) col)]) hashes)])]))
       (reduce (fn [[to th] [Os hashes]]
                 [(apply conj to Os) (apply conj th hashes)])
               [[] []])
       (map sort)
;;   now coordinates are rotated ccw
       (map (partial group-by (fn [[c r]] c)))
       ((fn [[Os-map hashes-map]]
          (mapv (fn [n]
                  [(mapv second (get Os-map n []))
                   (mapv second (get hashes-map n []))])
                (range (inc dim)))))))

(defn cycle-ref
  "Performs a full cycle starting from N going W S E. Not sure why the reserve is needed, but..."
  [dim arg]
  (->> arg (mapv tilt-numbers)
       (rotate-numbers dim false)
       (map tilt-numbers)
       (rotate-numbers dim true)
       (map tilt-numbers)
       (rotate-numbers dim true)
       (rseq)
       (map tilt-numbers)
       (rotate-numbers dim false)
       (rseq)))

(defn- cycle-times
  "Store time seen a nexr relector and move forward in time"
  [pi times]
  (let [mem (atom {})
        max-dim (dec (count pi))
        initial (map (fn [column]
                       [(rocks-at-col column "O") (rocks-at-col column "#")])
                     pi)]
    (loop [t 1
           refl initial
           traveled-ahead? false]
      (when (zero? (mod t 1))
        (prn "TIME " t))
      (if (> t times)
        refl

        (let [next-refl (cycle-ref max-dim refl)
              mem-key next-refl
              seen-before (get @mem mem-key)]
          (if (or traveled-ahead? (not seen-before))
            (let [next-refl (cycle-ref max-dim refl)]
              (swap! mem (fn [m] (assoc m mem-key t)))
              (recur (inc t) next-refl traveled-ahead?))

            (let [time-seen seen-before
                  dt (- t time-seen)
                  remaining-time (- times t)
                  repeat-so-many-times (quot remaining-time dt)
                  t' (+ t (* repeat-so-many-times dt))]
              (recur (inc t') next-refl true))))))))

(defn answer-2 [input times]
  (let [pi (parse input)
        len (count (first pi))]
    (total-load (cycle-times pi times) len)))

(is (= 64 answer-2 input))
(answer-2 (slurp "src/advent/2023/d14/input.txt") 1000000000)



(comment
;;   perform a few cycles
  (let [pi (parse input)
        dim (dec (count pi))]
    (->> pi
         (map (fn [column] [(rocks-at-col column "O") (rocks-at-col column "#")]))
         (cycle-ref dim)
         (cycle-ref dim)
         (cycle-ref dim)
       ;
         ))

;; Rotate to west and tilt
;; OOO0.#O...
;; OO..#....#
;; OO0..##0..
;; O..#OO....
;; ........#.
;; ..#....#.#
;; 0....#OO..
;; O.........
;; #....###..
;; #....#....

;; Rotate to south and tilt
;; .....#....
;; ....#.O..#
;; O..O.##...
;; O.O#......
;; O.O....O#.
;; O.#..O.#.#
;; 0....#....
;; OO....OO..
;; #O...###..
;; #O..O#....
;;   
  )
