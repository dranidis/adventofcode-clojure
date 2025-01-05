(ns advent.2022.d22.core
  (:require
   [advent.util :refer [coords-of-symbol draw-grid-condenced grid-2d set-grid
                        str->2D-no-trim]]
   [clojure.string :as str]
   [clojure.term.colors :as colors]))

(def example? false)

(def example "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(def input (if example? example (slurp "src/advent/2022/d22/input.txt")))
(def sections (str/split input #"\n\n"))

;; GRID
(def grid (str->2D-no-trim (first sections)))
(def rows (count grid))
(def cols (apply max (map count grid)))

(def face-size (->> grid
                    (map (fn [r] (remove #(= % " ") r)))
                    (map count)
                    (apply min)))

(def path (->> (second sections)
               (re-seq #"\d+|R|L")
               (mapv #(if-let [n (parse-long %)] n %))))

(def walls (coords-of-symbol grid "#"))
(def opens (coords-of-symbol grid "."))


(def tiles (vec (sort (concat opens walls))))
(def walls-set (set walls))
(def tiles-set (set tiles))

(def start [(first opens) 0])

(defn password [[[r c] d]]
  (prn r c d)
  (+ (* 1000 (inc r))
     (* 4 (inc c))
     d))

(defn- after-last-col [r c d]
  [(first (filter #(= r (first %)) tiles)) d])

(defn- after-last-row [r c d]
  [(first (filter #(= c (second %)) tiles)) d])

(defn- before-first-col [r c d]
  [(last (filter #(= r (first %)) tiles)) d])

(defn- before-first-row [r c d]
  [(last (filter #(= c (second %)) tiles)) d])

(defn- nxt [[[r c :as pos] d]]
  (case d
    0 (let [nrc (mapv + pos [0 1])]
        (if (tiles-set nrc)
          [nrc d]
          (after-last-col r c d)))
    1 (let [nrc (mapv + pos [1 0])]
        (if (tiles-set nrc)
          [nrc d]
          (after-last-row r c d)))
    2 (let [nrc (mapv + pos [0 -1])]
        (if (tiles-set nrc)
          [nrc d]
          (before-first-col r c d)))
    3 (let [nrc (mapv + pos [-1 0])]
        (if (tiles-set nrc)
          [nrc d]
          (before-first-row r c d)))
    (throw (ex-info (str "Inv dir: " d) {}))))

(defn d-g [[rc d] path]
  (let [path (loop [path (vec path)
                    D #{}
                    latest (list)]
               (if (empty? path)
                 (vec latest)
                 (let [[pos d] (peek path)]
                   (if (D pos)
                     (recur (pop path) D latest)
                     (recur (pop path) (conj D pos) (conj latest [pos d]))))))]
    (-> (grid-2d rows cols " ")
        (set-grid walls (colors/red "#"))
        (set-grid opens (colors/green "."))
        (set-grid (map first (filter (fn [[pos d]] (= d 0)) path)) ">")
        (set-grid (map first (filter (fn [[pos d]] (= d 1)) path)) "v")
        (set-grid (map first (filter (fn [[pos d]] (= d 2)) path)) "<")
        (set-grid (map first (filter (fn [[pos d]] (= d 3)) path)) "^")
        (set-grid [rc] (-> (case d 0 ">" 1 "v" 2 "<" 3 "^") colors/blink))
        (draw-grid-condenced))))

(defn- final-pos []
  (loop [[rc d :as rcd] start
         path path
         trace [start] ;; only necessary for drawing the grid
         ]
    (if (empty? path)
      rcd
      (let [f (first path)
            [n tr] (cond
                     (number? f) (loop [rcd rcd
                                        steps f
                                        tr []]
                                  ;;  (d-g rcd (vec (concat trace tr))) ;; comment out for drawing
                                   (if (zero? steps)
                                     [rcd tr]
                                     (let [n (nxt rcd)]
                                       (if (walls-set (first n))
                                         [rcd tr]
                                         (recur n (dec steps) (conj tr n))))))
                     (= f "R") [[rc (mod (inc d) 4)] []]
                     (= f "L") [[rc (mod (dec d) 4)] []]
                     :else (throw (ex-info "ERROR" {})))
            trace (apply conj trace tr)]
        (recur n
               (rest path)
               trace)))))

(println "ANS 1: " (password (final-pos)))

(def cube-net-tile-origins
  (vec (for [xr (range 4)
             xc (range 4)
             :let [cube-orig [(* face-size xr) (* face-size xc)]]
             :when (tiles-set cube-orig)]
         cube-orig)))

(defn to-relative-coords [tile [r c]]
  (mapv - [r c] (cube-net-tile-origins tile)))

(defn to-absolute-coords [tile [r c]]
  (mapv + [r c] (cube-net-tile-origins tile)))

(defn to-coords-fn
  "Converts the absolute coordinates r c of face `from`
   to the absolute coordinates of face `to`,
   using a function with relative transformation"
  [from to rel-fn]
  (fn [r c]
    (let [[rr rc] (to-relative-coords from [r c])
          [rpos d] (rel-fn rr rc)
          apos (to-absolute-coords to rpos)]
      (assert (and (>= (first apos) 0) (>= (second apos) 0))
              (str "F" from "T" to "RC"  [r c] "R" rpos "A" apos))
      [apos d])))

(defn inv [x] (dec (- face-size x)))
(defn max-f [n] (dec (* n face-size)))
(def max-s (max-f 1))

(defn- right->right [r c] [[(inv r) max-s] 2])
(defn- right->bottom [r c] [[max-s r] 3])
(defn- right->top [r c] [[0 (inv r)] 1])

(defn- bottom->right [r c] [[c max-s] 2])
(defn- bottom->top [r c] [[0 c] 1])
(defn- bottom->bottom [r c] [[max-s (inv c)] 3])
(defn- bottom->left [r c] [[(inv c) 0] 0])

(defn- left->left [r c] [[(inv r) 0] 0])
(defn- left->top [r c] [[0 r] 1])
(defn- left->bottom [r c] [[max-s (inv r)] 3])

(defn- top->left [r c] [[c 0] 0])
(defn- top->bottom  [r c] [[max-s c] 3])
(defn- top->right [r c] [[(inv c) max-s] 2])
(defn- top->top [r c] [[0 (inv c)] 1])

(def from-face-to-face
  "A mapping from [leaving-cube leaving-direction]
   to [destination-cube trans-function]
   The trans-function is a function from old [r c]
   to the new [r c] and direction.

   This part is hard-coded for the cubes in the example and input!"
  (if example?
    {[0 0] [5 right->right]
     [3 0] [5 right->top]
     [5 0] [0 right->right]
     [1 1] [4 bottom->bottom]
     [2 1] [4 bottom->left]
     [4 1] [1 bottom->bottom]
     [5 1] [1 bottom->left]
     [0 2] [2 left->top]
     [1 2] [5 left->bottom]
     [4 2] [2 left->bottom]
     [1 3] [0 top->top]
     [2 3] [0 top->left]
     [0 3] [1 top->top]
     [5 3] [3 top->right]}
    {[1 0] [4 right->right]
     [2 0] [1 right->bottom]
     [4 0] [1 right->right]
     [5 0] [4 right->bottom]
     [5 1] [1 bottom->top]
     [4 1] [5 bottom->right]
     [1 1] [2 bottom->right]
     [0 2] [3 left->left]
     [2 2] [3 left->top]
     [3 2] [0 left->left]
     [5 2] [0 left->top]
     [3 3] [2 top->left]
     [0 3] [5 top->left]
     [1 3] [5 top->bottom]}))

(defn leaving-cube-with-dir-fn [from dir]
  (let [[t f] (from-face-to-face [from dir])]
    (to-coords-fn from t f)))

(defn cube-at [r c]
  (let [cubes-at-rc
        (for [[cube-index [r-orig c-orig]]
              (map-indexed vector cube-net-tile-origins)

              :when (and (<= r-orig r (dec (+ r-orig face-size)))
                         (<= c-orig c (dec (+ c-orig face-size))))]
          cube-index)]
    (if (empty? cubes-at-rc) nil (first cubes-at-rc))))

;; REDEFINING nxt for part 2
(defn- nxt [[[r c :as pos] d]]
  (let [nrc (mapv + pos (nth [[0 1] [1 0] [0 -1] [-1 0]] d))]
    (if (tiles-set nrc)
      [nrc d]
      ((leaving-cube-with-dir-fn (cube-at r c) d) r c))))

(println "ANS 2: " (password (final-pos)))

