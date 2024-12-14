(ns advent.2021.d20.core
  (:require
   [advent.util :refer [draw-grid str->2D]]
   [clojure.string :as str]))

;; (comment
(def input
  "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")
  ;
  ;; )

(def input (slurp "src/advent/2021/d20/input.txt"))

(def image-enhancement-section (first (str/split input #"\n\n")))
(def image-section (second (str/split input #"\n\n")))
(def algorithm (str/split image-enhancement-section #""))

(defn parse-binary
  "Parse a string as a binary number."
  [binary-str]
  ;; (println binary-str (Integer/parseInt binary-str 2))
  (Integer/parseInt binary-str 2))

;; Example usage:
;; (parse-binary "1010") ;; => 10
;; (parse-binary "1111") ;; => 15

(defn grid9x9->bin->enhanced
  [grid r c background]
  (get algorithm
       (parse-binary
        (apply str
               (for [dr [-1 0 1]
                     dc [-1 0 1]]
                 (if-let [c (get-in grid [(+ r dr) (+ c dc)])]
                   (if (= c "#") 1 0)
                   background))))))

(defn enhance
  [grid background]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [r -2
           c -2
           new-row []
           new-grid []]
      ;; (print [r c] "")
      (if (>= r (+ rows 2))
        new-grid
        (if (>= c (+ cols 2))
          (recur (inc r) -2 [] (conj new-grid new-row))
          (recur r (inc c) (conj new-row (grid9x9->bin->enhanced grid r c background))
                 new-grid))))))

(defn enhance-times-1
  [image-section times]
  (loop [grid (str->2D image-section)
         background 0
         t 0]
    ;; (draw-grid grid)
    ;; (println "")
    (if (= t times)
      grid
      (recur (enhance grid background) 0 (inc t)))))

(defn enhance-times-2
  [image-section times]
  (loop [grid (str->2D image-section)
         background 0
         t 0]
    ;; (draw-grid grid)
    ;; (println "")
    (if (= t times)
      grid
      (recur (enhance grid background) (if (zero? background) 1 0) (inc t)))))

;; (parse-binary (apply str (map #(if (= % "#") 1 0) (str/split ".#......#" #""))))
;; (get-in (str->2D image-section) [1 3])
;; (get algorithm (parse-binary
;;                 (apply str (map #(if (= % "#") 1 0) (str/split ".#......#" #"")))))

(println "ANS 1" (count (filter #(= "#" %) (apply concat (enhance-times-2 image-section 2)))))

(println "ANS 2" (count (filter #(= "#" %) (apply concat (enhance-times-2 image-section 50)))))


;; apply the image enhancement algorithm twice
;; How many pixels are lit in the resulting image?
;; 6173 Too high
;; 5772 too high
;; 5309 too high
;; 5931