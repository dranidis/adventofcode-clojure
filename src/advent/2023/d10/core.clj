(ns advent.2023.d10.core
  (:require [clojure.string :as str]))

(def input (slurp "src/advent/2023/d10/input.txt"))

(defn parse-line [line]
  (str/split line #""))

(defn parse [example]
  (vec (for [line (str/split-lines example)]
         (parse-line line))))

(defn get-pipe [maze [x y]]
  (get-in maze [y x]))

(defn start [maze]
  (let [lines (count maze)
        cols (count (first maze))]
    (loop
     [y 0
      x 0]
      (if (>= y lines)
        :not-found
        (if (>= x cols)
          (recur (inc y) 0)
          (if (= "S" (get-in maze [y x]))
            [x y]
            (recur y (inc x))))))))

(defn neighbors [maze [x y]]
  (let [max-y (dec (count maze))
        max-x (dec (count (first maze)))]
    (for [[dx dy] [[-1 0]  [1 0] [0 1] [0 -1]]
          :let [new-x (+ x dx) new-y (+ y dy)
                pipe (get-pipe maze [new-x new-y])]
          :when (and  (<= 0 new-x max-x)
                      (<= 0 new-y max-y)
                      (not= "." pipe))
          :when (or (not (< new-y y)) (not (#{"-" "L" "J"} pipe)))
          :when (or (not (> new-y y)) (not (#{"-" "F" "7"} pipe)))
          :when (or (not (< new-x x)) (not (#{"|" "7" "J"} pipe)))
          :when (or (not (> new-x x)) (not (#{"|" "L" "F"} pipe)))]
      [new-x new-y])))

(defn next-co [maze [fromx fromy] [x y]]
  (case (get-pipe maze [x y])
    "|" (if (> y fromy) ; came from top
          [x (inc y)]
          [x (dec y)])
    "-" (if (> x fromx); came from left
          [(inc x) y]
          [(dec x) y])
    "L" (if (> y fromy) ; came from top
          [(inc x) y]
          [x (dec y)])
    "J" (if (> y fromy) ;  came from top
          [(dec x) y]
          [x (dec y)])
    "7" (if (< y fromy) ; came from down
          [(dec x) y]
          [x (inc y)])
    "F" (if (< y fromy) ; came from down
          [(inc x) y]
          [x (inc y)])
    "." :error
    "S" "S"
    "??????"))

(defn to-S-path [maze a-neighbor]
  (loop [from (start maze)
         to a-neighbor
         count 0
         path [(start maze) a-neighbor]]
    (let [next (next-co maze from to)]
      (if (= "S" next)
        path
        (recur to next (inc count) (conj path next))))))

(defn path [maze]
  (to-S-path maze (first (neighbors maze (start maze)))))

;; answer 1 simpler 
(defn answer-1 [example]
  (quot (count (path (parse example))) 2))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  part 2
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-in-between
  [[[x y] [x1 y1]]]
  (if (= x x1)
    [[x y]
     [x (+ y (/ (- y1 y) 2))]]
    [[x y]
     [(+ x (/ (- x1 x) 2)) y]]))

(defn double-pair [[x y]]
  [(* 2 x) (* 2 y)])

(defn double-the-path [the-path]
  (apply concat
         (map add-in-between
              (partition 2 1
                         (map double-pair the-path)))))

(defn neighbors-for-ff
  [the-expanded-path max-x max-y area-set [x y]]
  (for [[dx dy] [[-1 0]  [1 0] [0 1] [0 -1]]
        :let [x' (+ x dx) y' (+ y dy)]
        :when (and  (<= -1 x' max-x) ;; for the outside area -1
                    (<= -1 y' max-y))
        :when (not ((set the-expanded-path) [x' y']))
        :when (not (area-set [x' y']))]
    [x' y']))

(defn floodfill [the-path first-or-second]
  (let [start# (. System (nanoTime))
        the-expanded-path (double-the-path the-path)
        max-y (apply max (map second the-expanded-path))
        max-x (apply max (map first the-expanded-path))
        inside-outside-pts (neighbors-for-ff the-expanded-path
                                             max-x max-y
                                             #{} (second the-expanded-path)) ; that's a halfpoint
        inside-point (first-or-second inside-outside-pts)]
    (loop [area-1 (transient #{inside-point})
           next-1 (transient [inside-point])
           counter 0]
      (when (zero? (mod counter 1000))
        (println "FF:" counter  "to examine" (count next-1) "node. Time passed:"
                 (/ (double (- (. System (nanoTime)) start#)) 1000000000.0) "secs"))
      (if (zero? (count next-1))
        (persistent! area-1)
        (let [new-neighbors (neighbors-for-ff the-expanded-path
                                              max-x max-y area-1
                                              (get next-1 (dec (count next-1))))
              new-area (reduce conj! area-1 new-neighbors)
              new-next (reduce conj! (pop! next-1) new-neighbors)]
          (if (some (fn [[x y]] (or (< x 0) (< y 0))) new-neighbors)
            nil
            (recur new-area new-next (inc counter))))))))

(defn remove-all-in-between-points [ff]
  (filter (fn [[x y]] (and (integer? x)
                           (integer? y)))
          (map (fn [[x y]] [(/ x 2) (/ y 2)]) ff)))

(defn points-inside-flood-fill [ff]
  (count (remove-all-in-between-points ff)))

(defn answer-2 [the-path]
  (let [ff-1 (floodfill the-path first)]
    (if (nil? ff-1)
      (do
        (println "... failed. Trying Floodfill on the other side...")
        (points-inside-flood-fill (floodfill the-path second)))
      (points-inside-flood-fill ff-1))))

;; 
;; Define which path to calculate
;; 
(def the-path (path (parse input)))

;; 
;;  Solved with two theorems
;; 
;; Shoelace Formula
;; https://www.theoremoftheday.org/GeometryAndTrigonometry/Shoelace/TotDShoelace.pdf
(defn shoelace [the-path]
  (/ (apply + (map (fn [[[x0 y0] [x1 y1]]] [[x0 y0] [x1 y1]]
                     (- (* x0 y1) (* x1 y0)))
                   (partition 2 1 the-path)))
     2))

;; Pick's theorem
;; https://en.wikipedia.org/wiki/Pick%27s_theorem
;; A = i + b/2 - 1
;; Area A, i num of integer points inside, b the number of points on the boundary
(defn reverse-picks [b area]
  (inc (- area (/ b 2))))

(defn math-answer2 [the-path]
  (reverse-picks (dec (count the-path)) (shoelace the-path)))

(defn -main [& _]
  (println "Using theorems (Shoelace and inverse Pick's): "
           (math-answer2 the-path))
  (println "Running floodfill... wait for about 15200 iterations (~500 secs)")
  (println "RESULT from floodfill" (answer-2 the-path)))

(-main)