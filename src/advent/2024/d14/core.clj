(ns advent.2024.d14.core
  (:require
   [advent.util :refer [draw-grid parse-lines-with-numbers]]))

(def example? false)

(def example
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(def input (if example? example
               (slurp "src/advent/2024/d14/input.txt")))

(def max-x (if example? 11 101))
(def max-y (if example? 7 103))

(def robots (parse-lines-with-numbers input))

(defn move
  [[x y dx dy]]
  (let [newx (+ x dx)
        newy (+ y dy)
        newx (if (or (< newx 0) (>= newx max-x))
               (mod newx max-x) newx)
        newy (if (or (< newy 0) (>= newy max-y))
               (mod newy max-y) newy)]
    [newx newy dx dy]))

(comment
  ;; only for example
  (= [4 1 2 -3] (move [2 4 2 -3]))
  (= [6 5 2 -3] (move (move [2 4 2 -3])))
  (= [8 2 2 -3] (move (move (move [2 4 2 -3]))))
  (= [10 6 2 -3] (move (move (move (move [2 4 2 -3])))))
  (= [1 3 2 -3] (move (move (move (move (move [2 4 2 -3]))))))
  ;
  )

(defn grid
  [robots]
  (let [r-set (set (mapv (fn [[x y _ _]]
                           [x y]) robots))]
    (loop [x 0
           y 0
           row []
           grid []]
      (if (= y max-y)
        grid
        (if (= x max-x)
          (recur 0 (inc y) [] (conj grid row))
          (recur (inc x) y (conj row (if (r-set [x y])
                                       "#"
                                       ".")) grid))))))

(def robots-after-100
  (loop [robots robots
         time 0]
    (if (= time 100)
      robots
      (recur (mapv move robots)
             (inc time)))))

(def answer-1
  (apply * (for [opx [< >]
                 opy [< >]]
             (count (filter (fn [[x y _ _]]
                              (and (opx x (quot max-x 2))
                                   (opy y (quot max-y 2))))
                            robots-after-100)))))

;; PART 2

;; Divide the grid into 9 regions
;; Count the number of robots in each region
;; Calculate the variance of the number of robots in each region
;; Move the robots
;; Print the grid and the time when the variance is increased
;; Visual inspection of the grid shows that the robots are forming a shape
;; 
;; Execution has to be stopped manually

(def regions (let [stepx (quot max-x 3)
                   stepy (quot max-y 3)]
               (for [fromx (range 0 max-x stepx)
                     fromy (range 0 max-y stepy)
                     :let [tox (min (+ fromx stepx) max-x)
                           toy (min (+ fromy stepy) max-y)]]
                 [fromx tox fromy toy])))


(defn region-points
  [robots [fromx tox fromy toy]]
  (let [robot-set (set (mapv (fn [[x y _ _]]
                               [x y]) robots))]
    (count (for [x (range fromx tox)
                 y (range fromy toy)
                 :when (robot-set [x y])]
             [x y]))))

(defn variance
  [data]
  (let [mean (/ (apply + data) (count data))]
    (/ (apply + (mapv (fn [x]
                        (Math/pow (- x mean) 2))
                      data))
       (count data))))

(defn variance-robots
  [robots]
  (variance (mapv (partial region-points robots) regions)))

;; long loop to find the time when the variance is increased
;; so that visual inspection shows the shape of the robots
;; Loop stops after 10000 iterations and no increase in variance
;; time 10000 was chosen arbitrarily
(def answer-2 (loop [robots robots
                     totaltime 0
                     time-from-previous-increased-variance 0
                     max-variance 0
                     shape-robots robots
                     found-at-time 0]
                (if (= time-from-previous-increased-variance 10000)
                  (do
                    (draw-grid (grid shape-robots))
                    (println "AFTER" found-at-time)
                    found-at-time)
                  (let [v (variance-robots robots)]
                    (if (> v max-variance)
                      (recur (mapv move robots)
                             (inc totaltime)
                             0
                             v
                             robots
                             totaltime)
                      (recur (mapv move robots)
                             (inc totaltime)
                             (inc time-from-previous-increased-variance)
                             max-variance
                             shape-robots
                             found-at-time))))))

(defn- -main [& _]
  (println "Day 14, Part 1:" answer-1)
  (println "Day 14, Part 2:" answer-2))

