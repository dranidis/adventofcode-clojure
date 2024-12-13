(ns advent.util
  (:require
   [clojure.string :as str]))

(defn str->nums [s]
  (mapv parse-long (re-seq #"-?\d+" s)))

(comment
  (str->nums "1 2 -3")
  (str->nums "1 2 --3")
  (str->nums "a1a2 2 --3")
  ;
  )

(defn parse-lines-with-numbers [input]
  (vec (for [line (str/split-lines input)]
         (str->nums line))))

(comment
  (parse-lines-with-numbers "1 2 3
                             4 5 6
                             7 8 9")
  ;
  )

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c (str/trim line)]
                (str c))))))

(comment
  (str->2D "...#...
            ..#.#..
           .#...#.")
  ;
  )


(defn str->2D-num
  "Read a string containing new-lines into a 2 dimensional vector of numbers"
  [input]
  (mapv (fn [line] (mapv parse-long line))
        (str->2D input)))

(comment
  (str->2D-num "123
               456
               789")
  ;
  )

(defn transpose [m]
  (apply mapv vector m))

(comment
  (transpose [["1" "2" "3"]
              ["4" "5" "6"]
              ["7" "8" "9"]])
  ;
  )

(defn in-grid?
  "Returns a function that checks if a point is INSIDE a grid
   with the given number of rows and columns"
  [rows cols]
  (fn [[r c]]
    (and (>= r 0) (>= c 0) (< r rows) (< c cols))))


(defn draw-grid
  "Draws a grid to the console.
   The grid is a 2D array of strings."
  [grid]
  (doseq [row grid]
    (doseq [cell row]
      (print cell))
    (println)))

(defn draw-grid-from-points
  "Draws a grid to the console.
   The grid is a 2D array of strings.
   Points is a sequence of [row col] pairs.
   rows and cols are the dimensions of the grid."
  [rows cols points]
  (let [grid (to-array-2d (vec (repeat rows (vec (repeat cols ".")))))]
    (doseq [[r c] points]
      (aset grid r c "#"))
    (draw-grid grid)))


(defn middle-value-of-vector [vect]
  (when-not (empty? vect)
    (vect (quot (count vect) 2))))

(comment
  (middle-value-of-vector [1 2 3 4 5])
  ;; 3
  ;
  )

(defn in-vector?
  "true if coll contains elm"
  [coll elm]
  (and (vector? coll)
       (some #(= elm %) coll)))

;; 
;; BFS find all paths from start to end
;; 
(defn find-all-paths-bfs
  "Find all paths from start to end using the
   next-fn for getting the next nodes from the
   current node. next-fn should return a vector and
   it could be a function or a map."
  [next-fn start end]
  (loop [queue [[start]]
         paths []]
    (if (empty? queue)
      paths
      (let [path (first queue)
            current (last path)]
        (if (= current end)
          (recur (rest queue) (conj paths path))
          (let [nexts (next-fn current)
                new-paths (map #(conj path %) nexts)]
            (recur (concat new-paths (rest queue)) paths)))))))

(comment
  (find-all-paths-bfs
   {:A [:B :C]
    :B [:C :D]
    :C [:D :E]
    :D [:E]
    :E []}
   :A :E)
  ;
  )

(defn flood-fill
  "Returns a set of points in the region.
   next-fn is a function that returns the list of next points to visit
   given a point [r c]."
  [next-fn r c]
  (set (loop [stack [[r c]]
              visited #{[r c]}]
         (if (empty? stack)
           visited
           (let [[r c] (first stack)
                 next (next-fn r c)
                 next (remove visited next)
                 visited (into visited next)
                 stack (concat (rest stack) next)]
             (recur stack visited))))))