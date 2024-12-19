(ns advent.util
  (:require
   [clojure.string :as str]
   [clojure.term.colors :as colors]))

(defn third
  [l]
  (nth l 2))

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

(defn parse-lines-with-parser [parser input]
  (vec (for [line (str/split-lines input)]
         (parser line))))

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

(defn str->2D-no-trim
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
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


(defn coords-of-symbol
  [grid symbol]
  (let [rows (count grid)
        cols (count (first grid))]
    (vec (for [r (range rows)
               c (range cols)
               :when (= (get-in grid [r c]) symbol)]
           [r c]))))

(defn coords-of-pred
  [grid pred]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [r (range rows)
          c (range cols)
          :when (pred (get-in grid [r c]))]
      [r c])))

(comment
  (def grid (str->2D "...#...
                        ..#.#..
                       .#...#."))

  (coords-of-symbol grid "#")
  ;; ([0 3] [1 2] [1 4] [2 1] [2 5])

  (coords-of-pred grid #(= % "#"))
  ;; ([0 3] [1 2] [1 4] [2 1] [2 5])

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

  (let [n (atom 0)
        rows (count grid)
        cols (count (first grid))]
    ;; print a line with the numbers 0123456...
    (print "    ")
    (doseq [c (range cols)]
      (let [q (quot c 10)]
        (print (if (zero? q) " " q))))
    (println)
    (print "    ")
    (doseq [c (range cols)]
      (print (mod c 10)))
    (println)
    (doseq [row grid]
      (let [q (quot @n 10)
            left-digits (str (if (zero? q) " " q) (mod @n 10))]
        (do (print left-digits " ")
            (swap! n inc)))
      (doseq [cell row]
        (print cell))
      (println))))

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

(defn grid-2d
  "Creates a two dimemsional grid of strings with the given number of rows and columns.
   The grid is initialized with the symbol everywhere.
   Other sets of points with their symbols can be added later
   with the set-grid function.
   
   ## Example:
   ```
   
  (-> (grid-2d 25 25 \\.)
      (set-grid [[1 1] [1 2] [2 0] [9 9]] \\O)
      (set-grid [[5 5] [4 6 ] [4 7] [4 9]] \\#)
      (draw-grid))
   ```

   "
  [rows cols symbol]
  (to-array-2d (vec (repeat rows (vec (repeat cols symbol))))))

(defn set-grid
  "Sets the symbols in the grid at the given points."
  [grid points symbol]
  (doseq [[r c] points]
    (aset grid r c symbol))
  grid)

(comment

  (-> (grid-2d 25 25 ".")
      (set-grid [[1 1] [1 2] [2 0] [9 9]] (colors/on-red "O"))
      (draw-grid))


  (-> (grid-2d 25 25 \.)
      (set-grid [[1 1] [1 2] [2 0] [9 9]] \O)
      (set-grid [[5 5] [4 6] [4 7] [4 9]] \#)

      (draw-grid))
  ;
  )


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

(defn flood-fill-rc
  [next-fn [r c]]
  (flood-fill next-fn r c))

(defn parse-binary
  "Parse a string as a binary number."
  [binary-str]
  (Integer/parseInt binary-str 2))