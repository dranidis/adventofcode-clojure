(ns advent.2024.d15.core2
  (:require
   [advent.util :refer [draw-grid grid-2d set-grid str->2D]]
   [clojure.string :as str]))

(def example? false)

(def example
  "")

(def input (if example? example (slurp "src/advent/2024/d15/input.txt")))

;; SECTIONS
(def map-section (first (str/split input #"\n\n")))
(def move-section (second (str/split input #"\n\n")))

;; GRID
(def grid (str->2D map-section))
(def rows (count grid))
(def cols (count (first grid)))



;; MOVES
(def moves (str/split (str/join (str/split-lines move-section)) #""))
;; (count moves)

(def robot (first (for [r (range rows)
                        c (range cols)
                        :when (= (get-in grid [r c]) "@")]
                    [r (* 2 c)])))

(def boxes (set (for [r (range rows)
                      c (range cols)
                      :when (= (get-in grid [r c]) "O")
                      box [{:a [r (* 2 c)] :b [r (inc (* 2 c))]}]]
                  box)))

(def walls (set (for [r (range rows)
                      c (range cols)
                      :when (= (get-in grid [r c]) "#")
                      wall [[r (* 2 c)] [r (inc (* 2 c))]]]
                  wall)))

(comment
  ;; You can use the following functions to draw the grid

  (defn- draw-all
    [walls boxes robot]
    (let [boxes (vec (reduce (fn [acc b] (concat acc [(:a b) (:b b)])) [] boxes))]
      (-> (grid-2d rows (* 2 cols) ".")
          (set-grid walls "#")
          (set-grid boxes "x")
          (set-grid [robot] "@")
          (draw-grid))))

  (draw-all walls boxes robot)
  ;
  )


(defn- box-hits-walls?
  [box]
  (some walls (vals box)))

(defn- move-box-by
  [box move]
  (case move
    "v" (-> box (update-in [:a 0] inc) (update-in [:b 0] inc))
    "^" (-> box (update-in [:a 0] dec) (update-in [:b 0] dec))
    ">" (-> box (update-in [:a 1] inc) (update-in [:b 1] inc))
    "<" (-> box (update-in [:a 1] dec) (update-in [:b 1] dec))))

(defn- get-box
  [boxes point]
  (filter (fn [b] (or (= (get b :a) point)
                      (= (get b :b) point)))
          boxes))

(defn- get-pushing-boxes
  [boxes box new-box]
  (let [res (disj (set (reduce (fn [acc b]
                                 (if-let [boxes (get-box boxes (get new-box b))]
                                   (apply conj acc boxes)))
                               #{}
                               [:a :b]))
                  box)]
    (if (empty? res) nil res)))

(defn- move-box
  "Parameters:
   - box: the box to move
   - move: the direction to move the box
   - boxes: the set of all boxes
   Returns: [new-boxes moved?]
   - new-boxes: the new set of updated boxes after the move
   - moved?: true if the box was moved, false otherwise"
  [box move boxes]
  (let [new-box (move-box-by box move)]
    (if (box-hits-walls? new-box)
      [boxes false]
      ;; get the boxes that are being pushed
      (if-let [pushed-boxes (get-pushing-boxes boxes box new-box)]

        ;; get the results of pushing all boxes
        (let [[new-boxes moved?]
              (reduce (fn [[boxes moved?] pushed-box]
                        (if moved?
                          (let [[new-boxes moved?] (move-box pushed-box move boxes)]
                            (if (not moved?)
                              [boxes false]
                              [new-boxes true]))
                          [boxes false]))
                      [boxes true]
                      pushed-boxes)]

          (if (not moved?)
            [boxes false]
            [(conj (disj new-boxes box) new-box) true]))
        ;; if no boxes are being pushed, just move the box
        [(conj (disj boxes box) new-box) true]))))

(comment
  ;; used for the assertions

  (defn- boxes-overlap?
    [boxes]
    (let [box-points (reduce (fn [acc b] (concat acc [(:a b) (:b b)])) [] boxes)]
      (not= (count box-points) (count (set box-points)))))

  (def box-count (count boxes))
;
  )

(defn new-boxes
  [moves robot boxes]
  (loop [moves moves
         robot robot
         boxes boxes]

    ;; (draw-all walls boxes robot)
    ;; (assert (= (count boxes) box-count))
    ;; (assert (not (boxes-overlap? boxes)))

    (if (empty? moves)
      boxes
      (let [move (first moves)
            [r c] robot
            new-robot (case move
                        "v" [(inc r) c]
                        "^" [(dec r) c]
                        ">" [r (inc c)]
                        "<" [r (dec c)])]
        (if (walls new-robot)
          (recur (rest moves) robot boxes)
          (if-let [box (first (get-box boxes new-robot))]
            (let [[new-boxes moved?] (move-box box move boxes)
                  new-rc (if moved? new-robot robot)]
              (recur (rest moves) new-rc new-boxes))
            (recur (rest moves) new-robot boxes)))))))

(def answer-2 (apply + (map
                        (fn [b] (+ (* 100 (get-in b [:a 0])) (get-in b [:a 1])))
                        (new-boxes moves robot boxes))))

(println answer-2)



