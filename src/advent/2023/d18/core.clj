(ns advent.2023.d18.core
  (:require [clojure.string :as str]))

(defn shoelace
  "Shoelace Formula
 https://www.theoremoftheday.org/GeometryAndTrigonometry/Shoelace/TotDShoelace.pdf"
  [the-path]
  (/ (apply + (map (fn [[[x0 y0] [x1 y1]]] [[x0 y0] [x1 y1]]
                     (- (* x0 y1) (* x1 y0)))
                   (partition 2 1 the-path)))
     2))

(defn reverse-picks
  "Uses inverse of Pick's theorem to calculate the points inside the boundary points.
https://en.wikipedia.org/wiki/Pick%27s_theorem
A = i + b/2 - 1
where A is the area, i num of integer points inside, b the number of points on the boundary"
  [b area]
  (inc (- area (/ b 2))))

(defn parse [input]
  (for [line (str/split-lines input)]
    (let [[[_ dir num color]] (re-seq #"(.) (\d+) \(#(.*)\)" line)]
      [dir (parse-long num) color])))

(defn answer [parse input]
  (let [pi (parse input)]
    (loop [pi pi
           boundary 0
           [x y] [0 0]
           points [[0 0]]]
      (if (seq pi)
        (let [[dir num _] (first pi)
              [dirx diry] (case dir "R" [1 0] "L" [-1 0] "D" [0 1] "U" [0 -1])
              new-x (+ x (* dirx num))
              new-y (+ y (* diry num))
              boundary (+ boundary num)
              points (conj points [new-x new-y])]
          (recur (rest pi) boundary [new-x new-y] points))
        (reverse-picks boundary (shoelace points))))))

;; part 2

(defn parse-2 [input]
  (for [line (str/split-lines input)]
    (let [[[_ _ _ color]] (re-seq #"(.) (\d+) \(#(.*)\)" line)
          last-char (subs color (dec (count color)))
          dir (case last-char "0" "R" "1" "D" "2" "L" "3" "U")
          first-chars (subs color 0 (dec (count color)))
          num (read-string (str "0x" first-chars))]
      [dir num color])))

(defn -main [& _]
  (time (println "Day 1, Part 1:" (answer parse (slurp "src/advent/2023/d18/input.txt"))))
  (time (println "Day 1, Part 2:" (answer parse-2 (slurp "src/advent/2023/d18/input.txt"))))) 

