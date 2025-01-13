(ns advent.util
  (:require
   [clojure.string :as str]
   [clojure.term.colors :as colors]
   [criterium.core :refer [quick-bench]]))

(defn third
  [l]
  (nth l 2))

(defn str->nums [s]
  (mapv parse-long (re-seq #"-?\d+" s)))

(defn str->num [s]
  (parse-long (re-find #"-?\d+" s)))

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
        cols (apply max (map count grid))]
    (vec (for [r (range rows)
               c (range cols)
               :when (= (get-in grid [r c]) symbol)]
           [r c]))))

(defn xy-of-symbol
  [grid symbol]
  (let [rows (count grid)
        cols (apply max (map count grid))]
    (vec (for [y (range rows)
               x (range cols)
               :when (= (get-in grid [y x]) symbol)]
           [x y]))))

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
        (print cell ""))
      (println))))

(defn draw-grid-condenced
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

(defn draw-hex
  "Draws a hex grid to the console.
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
    (print "    " (apply str (repeat rows " ")))
    (doseq [c (range cols)]
      (print (str (mod c 10) " ")))
    (println)
    (doseq [row grid]
      (let [q (quot @n 10)
            left-digits (str (if (zero? q) " " q) (mod @n 10))]
        (do (print left-digits " " (apply str (repeat (- rows @n) " ")))
            (swap! n inc)))
      (doseq [cell row]
        (print cell ""))
      (println))))

(defn draw-grid-from-points
  "Draws a grid to the console.
   The grid is a 2D array of strings.
   Points is a sequence of [row col] pairs.
   rows and cols are the dimensions of the grid."

  [rows cols points]
  (println rows cols)
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

  (-> (grid-2d 25 25 \.)
      (set-grid [[1 1] [1 2] [2 0] [9 9]] \O)
      (set-grid [[5 5] [4 6] [4 7] [4 9]] \#)

      (draw-grid-condenced))

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

(defn in-list? [lst elem]
  (some #(= elem %) lst))
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
  (Long/parseLong binary-str 2))

(defn long->bin-str [n] (Long/toString n 2))
(defn long->bin-vec [n]
  (mapv #(parse-long (str %)) (vec (long->bin-str n))))

(comment
  (parse-binary "101")
  ;;=> 5

  (long->bin-str 8)
  ;; "1000"

  (long->bin-vec 8)
  ;; [1 0 0 0]
  )

(defn bin-vec->long [b] (Long/parseLong (apply str b) 2))

(comment
  (bin-vec->long [1 0 0 0])
  ; 8
  )

;; COUNTER

(defn counter [] {})
;; Use get as in a map to get the times an element appears
;; Remove an element with dissoc

(defn add-to-counter [counter item]
  (update counter item (fnil inc 0)))

(defn rm-from-counter [counter item]
  (if (= 1 (get counter item))
    (dissoc counter item)
    (update counter item (fnil dec 0))))

(defn add-to-counter-times [counter item n]
  (update counter item (fnil #(+ % n) 0)))

(defn add-items-to-counter [counter items]
  (reduce #(add-to-counter %1 %2) counter items))

(defn add-items-to-counter-times [counter items n]
  (reduce #(add-to-counter-times %1 %2 n) counter items))

(defn count-counter [counter]
  (->> counter (map second) (apply +)))

(comment
  (-> (counter) (add-to-counter 1) (add-to-counter-times 1 3) (add-to-counter 2))
  ;;=> {1 4, 2 1}

  (-> (counter) (add-to-counter 1) (add-to-counter 1) (add-to-counter 2) first)
  ;;=> [1 2]

  (-> (counter) (add-to-counter 1) (add-to-counter 1) (add-to-counter 2) (dissoc 1))
  ;;=> {2 1}

  (-> (counter) (add-to-counter "a") (get "a"))
  ;;=> 1

  (-> (counter) (add-to-counter ">") (add-to-counter ">")
      (rm-from-counter ">"))
  ;;=> {">" 1}

  (-> (counter) (add-to-counter ">") (add-to-counter ">")
      (rm-from-counter ">") (rm-from-counter ">"))
  ;;=> {}

  (-> (counter)
      (add-to-counter "a")
      (add-to-counter "b")
      (dissoc "a"))
  ;;=> {"b" 1}

  (add-items-to-counter (counter) [1 2 3])
  ;;=> {1 1, 2 1, 3 1}

  (add-items-to-counter-times (counter) [1 2 3] 3)
  ;;=> {1 3, 2 3, 3 3}
  )

(defn chinese-remainder-theorem-naive
  "Receives as an argument a list of [r_i N_i] pairs
   and finds the x where
   x is congruent to r1 (mod N1) etc.
   That is if we take x mod N we get r1.
   Example:
   
     To find x where 
     -  x mod 5 = 3, 
     -  x mod 7 = 1
     -  x mod 8 = 6
   
   call with (chinese-remainter-theorem [[3 5] [1 7] [6 8]])"
  [r-mods]
  (let [r_is (mapv #(nth % 0) r-mods)
        M_is (mapv #(nth % 1) r-mods)
        M (apply * M_is)
        Ms (mapv #(quot M %) M_is)
        ;; uses linear search to find x_i (s)
        ;; might not be efficient for large numbers
        x_is (mapv (fn [M m]
                     (let [Mm (mod M m)]
                       (first
                        (for [x (range)
                              :when (= 1 (mod (* Mm x) m))]
                          x))))
                   Ms M_is)
        rMxs (map * r_is Ms x_is)]
    (mod (apply + rMxs) M)))

(comment
  ;; https://www.youtube.com/watch?v=zIFehsBHB8o
  ;; find x where 
  ;;     x mod 5 = 3, 
  ;;     x mod 7 = 1
  ;;     x mod 8 = 6
  ;; the numbers 5 7 8 must be co-primes. If they are primes, then they are co-primes.
  (def r-mods [[3 5] [1 7] [6 8]])

  (chinese-remainder-theorem-naive r-mods)
  ;;=> 78

  ;
  )

(defn gcd
  "Greatest common divisor"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(comment
  (gcd 25 120)
  ;
  )

(defn lcm
  "Least common multiple"
  [a b]
  (/ (* a b) (gcd a b)))

(comment
  (lcm 25 120)
  ;
  )

(defn mod-inverse
  "Finds the modular multiplicative inverse of a under modulo m."
  [a m]

  (letfn [(egcd [a b]
            (if (zero? b)
              [a 1 0]
              (let [[g x y] (egcd b (mod a b))]
                [g y (- x (* y (quot a b)))])))]
    (let [[g x _] (egcd a m)]
      (if (not= g 1)
        (throw (ex-info "Inverse does not exist" {:a a :m m}))
        (mod x m)))))

(defn chinese-remainder-theorem
  "Receives as an argument a list of [r_i N_i] pairs
     and finds the x where
     x is congruent to r1 (mod N1) etc.
     That is if we take x mod N we get r1.
     Example:
     
       To find x where 
       -  x mod 5 = 3, 
       -  x mod 7 = 1
       -  x mod 8 = 6
     
     call with (chinese-remainter-theorem [[3 5] [1 7] [6 8]])"
  [pairs]

  (let [N (reduce * (map second pairs))  ; Product of all moduli
        terms (map (fn [[r_i N_i]]
                     (let [n_i (/ N N_i)
                           inv-n_i (mod-inverse n_i N_i)]
                       (* r_i n_i inv-n_i)))
                   pairs)]
    (mod (reduce + terms) N)))

(comment
;; Example usage:
  (def example [[3 5] [1 7] [6 8]])
  (chinese-remainder-theorem example)
;;=> 78
  )

(defn manhattan-distance [[s1 s2] [b1 b2]]
  (+ (abs (- s1 b1)) (abs (- s2 b2))))

(defn manhattan-distance-3d [[s1 s2 s3] [b1 b2 b3]]
  (+ (abs (- s1 b1)) (abs (- s2 b2)) (abs (- s3 b3))))