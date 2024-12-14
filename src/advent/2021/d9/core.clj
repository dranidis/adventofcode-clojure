(ns advent.2021.d9.core
  (:require
   [advent.util :refer [str->2D-num]]))

(comment
  (def input "")
 ;
  )

(def input (slurp "src/advent/2021/d9/input.txt"))

(def grid (str->2D-num input))
(def cols (count (first grid)))
(def rows (count grid))

(defn low-point?
  [r c]
  (every? #(> % (get-in grid [r c]))
          (remove nil?
                  (for [dir [[0 1] [1 0] [0 -1] [-1 0]]]
                    (get-in grid [(+ r (first dir)) (+ c (second dir))])))))

(def low-points
  (for [r (range 0 rows)
        c (range 0 cols)
        :when (low-point? r c)]
    [r c]))

(def answer-1 (apply + (map #(inc (get-in grid %)) low-points)))

(defn basin-of-low-point
  [[r c]]
  (loop [queue [[r c]]
         visited #{[r c]}]
    (if (empty? queue)
      (count visited)
      (let [[r c] (first queue)
            queue (rest queue)
            neighbors (for [dir [[0 1] [1 0] [0 -1] [-1 0]]]
                        [(+ r (first dir)) (+ c (second dir))])
            neighbors (filter (fn [[r c]]
                                (and (not (visited [r c]))
                                     (not (nil? (get-in grid [r c])))
                                     (not= 9 (get-in grid [r c]))))
                              neighbors)]
        (recur (concat queue neighbors)
               (reduce conj visited neighbors))))))

(def answer-2 (apply * (take 3 (sort > (map basin-of-low-point low-points)))))


(defn -main [& _]
  (println "Day 9, Part 1:" answer-1)
  (println "Day 9, Part 2:" answer-2))

(-main)

