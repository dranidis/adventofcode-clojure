(ns advent.2021.d22.core
  (:require
   [advent.util :refer [parse-lines-with-parser str->nums]]
   [clojure.test :refer [is testing]]))

(def example? true)

(def example
  "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

(def input (if example? example (slurp "src/advent/2021/d22/input.txt")))

;; ONLY NUMBERS

(defn line-parser [line]
  (let [[[_ state xys]] (re-seq #"(on|off)(.*)" line)]
    [state (mapv vec (partition 2 (str->nums xys)))]))

(def parsed (parse-lines-with-parser line-parser input))

(defn on?
  [x y z]
  (loop [parsed parsed
         state 0]
    (if (empty? parsed)
      state
      (let [[cmd [fx tox fy toy fz toz]] (first parsed)
            new-state (if (and (<= fx x tox)
                               (<= fy y toy)
                               (<= fz z toz))
                        (if (= cmd "on") 1 0)
                        state)]
        (recur (rest parsed) new-state)))))


;; ;; Brute force
;; (def answer-1 (count (for [x (range -50 51)
;;                            y (range -50 51)
;;                            z (range -50 51)
;;                            :when (= 1 (on? x y z))]
;;                        [x y z])))


;; A one-dimensional multi-range of integer numbers is represented as a list
;; of distinct (not overlapping) sorted ranges [[fx1 tox1] [fx2 tox2] ... [fxn toxn]]
;; where fx1 <= tox1 < (tox1 + 1) < fx2 <= tox2 < (tox2 + 1) ... < fxn <= toxn
;
;; Two ranges can be added with the add-ranges function.
;; The result is a list of distinct (not overlapping) sorted ranges.
(defn add-ranges
  [range1 range2]
  ;; (prn "Adding ranges" range1 range2)
  (let [ranges (sort (concat range1 range2))]
    (loop [ranges ranges
           new-ranges []]
      (if (empty? ranges)
        new-ranges
        (let [[fx1 tox1] (first ranges)
              [fx2 tox2] (second ranges)]
          (if (or (nil? fx2) (> fx2 (inc tox1)))
            (recur (rest ranges) (conj new-ranges [fx1 tox1]))
            (recur (cons [(min fx1 fx2) (max tox1 tox2)] (drop 2 ranges)) new-ranges)))))))

(testing "add-ranges"
  (is (= [[1 8]] (add-ranges [[1 3] [5 7]] [[2 4] [6 8]])))
  (is (= [[1 3] [5 10] [12 14]] (add-ranges [[1 3] [5 7]] [[8 10] [12 14]])))
  (is (= [[-1 10]] (add-ranges [[1 3] [5 7]]  [[-1 10]]))))

(defn subtract-range
  "Subtract a single range from another range."
  [[fx1 tox1] [fx2 tox2]]
  (cond
    ;; No overlap
    (or (< tox2 fx1) (< tox1 fx2)) [[fx1 tox1]]
    ;; Complete overlap
    (and (<= fx2 fx1) (>= tox2 tox1)) []
    ;; Partial overlap on the left
    (and (<= fx2 fx1) (< tox2 tox1)) [[(inc tox2) tox1]]
    ;; Partial overlap on the right
    (and (> fx2 fx1) (>= tox2 tox1)) [[fx1 (dec fx2)]]
    ;; Overlap in the middle
    :else [[fx1 (dec fx2)] [(inc tox2) tox1]]))

(testing "subtract-range"
;; Test cases for subtract-range
  (is (= (subtract-range [1 5] [3 4]) [[1 2] [5 5]])) ;; Partial overlap in the middle
  (is (= (subtract-range [1 5] [0 2]) [[3 5]])) ;; Partial overlap on the left
  (is (= (subtract-range [1 5] [4 6]) [[1 3]])) ;; Partial overlap on the right
  (is (= (subtract-range [1 5] [1 5]) [])) ;; Complete overlap
  (is (= (subtract-range [1 5] [6 8]) [[1 5]])) ;; No overlap
  (is (= (subtract-range [1 5] [0 0]) [[1 5]])) ;; No overlap (range before)
  (is (= (subtract-range [1 5] [6 6]) [[1 5]])) ;; No overlap (range after)
  (is (= (subtract-range [1 5] [0 1]) [[2 5]])) ;; Partial overlap on the left edge
  (is (= (subtract-range [1 5] [5 6]) [[1 4]])) ;; Partial overlap on the right edge
  (is (= (subtract-range [1 5] [2 3]) [[1 1] [4 5]])) ;; Partial overlap in the middle
  )

;; Multi-ranges can also be substracted with the substract-ranges function.
;; The result is a list of distinct (not overlapping) sorted ranges
;; in which the substracted ranges are removed.

(defn subtract-ranges
  "Subtract a list of ranges from another list of ranges."
  [range1 range2]
  (reduce
   (fn [result r2]
     (mapcat #(subtract-range % r2) result))
   range1
   range2))

(testing "subtract-ranges";; Multiple ranges
  (is (= (subtract-ranges [[1 5] [10 15]] [[3 4] [12 13]])
         [[1 2] [5 5] [10 11] [14 15]])) ;; Multiple ranges with partial overlaps
  (is (= (subtract-ranges [[1 10]] [[3 7]])
         [[1 2] [8 10]])) ;; Single range with partial overlap in the middle
  (is (= (subtract-ranges [[1 10] [15 20]] [[5 17]])
         [[1 4] [18 20]])) ;; Multiple ranges with a large overlapping range
  (is (= (subtract-ranges [[1 5] [10 15]] [[0 20]])
         [])) ;; Complete overlap with a large range
  (is (= (subtract-ranges [[1 5] [10 15]] [[6 9]])
         [[1 5] [10 15]])) ;; No overlap with a range in between
  )


(defn add-ranges-1d
  "Add two 1D ranges."
  [range1 range2]
  (let [ranges (sort (concat range1 range2))]
    (loop [ranges ranges
           new-ranges []]
      (if (empty? ranges)
        new-ranges
        (let [[fx1 tox1] (first ranges)
              [fx2 tox2] (second ranges)]
          (if (or (nil? fx2) (> fx2 (inc tox1)))
            (recur (rest ranges) (conj new-ranges [fx1 tox1]))
            (recur (cons [(min fx1 fx2) (max tox1 tox2)] (drop 2 ranges)) new-ranges)))))))

(defn add-ranges-3d
  "Add two lists of 3D ranges."
  [ranges1 ranges2]
  (let [all-ranges (concat ranges1 ranges2)]
    ;; (prn all-ranges)
    (reduce
     (fn [result range]
       (let [[[fx1 tox1] [fy1 toy1] [fz1 toz1]] range]
         (vec (for [x (add-ranges (mapv first result) [[fx1 tox1]])
              ;;  :let [_ (prn "x range" x)]
                    y (add-ranges (mapv second result) [[fy1 toy1]])
              ;;  :let [_ (prn "y range" y)]

                    z (add-ranges (mapv #(nth % 2) result) [[fz1 toz1]])
              ;;  :let [_ (prn "z range" z)]
                    ]
                [x y z]))))
     []
     all-ranges)))

(is (= (add-ranges-3d [[[1 3] [1 3] [1 3]]]
                      [[[2 4] [2 4] [2 4]]])
       [[[1 4] [1 4] [1 4]]]))

(is (= (add-ranges-3d [[[1 3] [1 3] [1 3]] [[5 7] [5 7] [5 7]]]
                      [[[2 4] [2 4] [2 4]] [[6 8] [6 8] [6 8]]])
       [[[1 8] [1 8] [1 8]]]))

(defn subtract-ranges-3d
  "Subtract a list of 3D ranges from another list of 3D ranges."
  [range1 range2]
  (reduce
   (fn [result r2]
     (mapcat
      (fn [[[fx1 tox1] [fy1 toy1] [fz1 toz1]]]
        (for [x (subtract-range [fx1 tox1] (first r2))
              y (subtract-range [fy1 toy1] (second r2))
              z (subtract-range [fz1 toz1] (nth r2 2))]
          [x y z]))
      result))
   range1
   range2))
(testing "subtract-ranges-3d"
;; Test cases for subtract-ranges-3d
  (is (= (subtract-ranges-3d [[[1 5] [1 5] [1 5]]]
                             [[[3 4] [3 4] [3 4]]])
         [[[1 2] [1 2] [1 2]]
          [[1 2] [1 2] [5 5]]
          [[1 2] [5 5] [1 2]]
          [[1 2] [5 5] [5 5]]
          [[5 5] [1 2] [1 2]]
          [[5 5] [1 2] [5 5]]
          [[5 5] [5 5] [1 2]]
          [[5 5] [5 5] [5 5]]]))

  (is (= (subtract-ranges-3d [[[1 10] [1 10] [1 10]]] [[[3 7] [3 7] [3 7]]])
         [[[1 2] [1 2] [1 2]] [[1 2] [1 2] [8 10]] [[1 2] [8 10] [1 2]] [[1 2] [8 10] [8 10]]
          [[8 10] [1 2] [1 2]] [[8 10] [1 2] [8 10]] [[8 10] [8 10] [1 2]] [[8 10] [8 10] [8 10]]]))

  (is (= (subtract-ranges-3d [[[1 5] [1 5] [1 5]] [[10 15] [10 15] [10 15]]]
                             [[[3 4] [3 4] [3 4]] [[12 13] [12 13] [12 13]]])
         [[[1 2] [1 2] [1 2]] [[1 2] [1 2] [5 5]] [[1 2] [5 5] [1 2]] [[1 2] [5 5] [5 5]]
          [[5 5] [1 2] [1 2]] [[5 5] [1 2] [5 5]] [[5 5] [5 5] [1 2]] [[5 5] [5 5] [5 5]]
          [[10 11] [10 11] [10 11]] [[10 11] [10 11] [14 15]] [[10 11] [14 15] [10 11]] [[10 11] [14 15] [14 15]]
          [[14 15] [10 11] [10 11]] [[14 15] [10 11] [14 15]] [[14 15] [14 15] [10 11]] [[14 15] [14 15] [14 15]]]))

  (is (= (subtract-ranges-3d [[[1 5] [1 5] [1 5]]]
                             [[[0 0] [0 0] [0 0]]])
         [[[1 5] [1 5] [1 5]]])) ;; No overlap

  (is (= (subtract-ranges-3d [[[1 5] [1 5] [1 5]]]
                             [[[6 6] [6 6] [6 6]]])
         [[[1 5] [1 5] [1 5]]])) ;; No overlap

  (is (= (subtract-ranges-3d [[[1 5] [1 5] [1 5]]]
                             [[[0 1] [0 1] [0 1]]])
         [[[2 5] [2 5] [2 5]]])) ;; Partial overlap on the left edge

  (is (= (subtract-ranges-3d [[[1 5] [1 5] [1 5]]]
                             [[[5 6] [5 6] [5 6]]])
         [[[1 4] [1 4] [1 4]]])) ;; Partial overlap on the right edge)
  )

(defn count-ons
  [ranges]
  (apply + (map (fn [[[x1 x2] [y1 y2] [z1 z2]]]
                  (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))
                ranges)))

(defn range-intersection
  "Return the intersection of two ranges."
  [[fx1 tox1] [fx2 tox2]]
  (let [sorted (sort [[fx1 tox1] [fx2 tox2]])
        [[fx1 tox1] [fx2 tox2]] sorted]
    (if (> fx2 tox1)
      nil
      [(max fx1 fx2) (min tox1 tox2)])))

(defn add-3d-range
  [])

(testing "range-intersection"
  (is (= (range-intersection [1 5] [3 7]) [3 5])) ;; Partial overlap in the middle
  (is (= (range-intersection [1 5] [0 2]) [1 2])) ;; Partial overlap on the left
  (is (= (range-intersection [1 5] [4 6]) [4 5])) ;; Partial overlap on the right
  (is (= (range-intersection [1 5] [1 5]) [1 5])) ;; Complete overlap
  (is (= (range-intersection [1 5] [6 8]) nil)) ;; No overlap
  (is (= (range-intersection [1 5] [0 0]) nil)) ;; No overlap (range before)
  (is (= (range-intersection [1 5] [6 6]) nil)) ;; No overlap (range after)
  (is (= (range-intersection [1 5] [0 1]) [1 1])) ;; Partial overlap on the left edge
  (is (= (range-intersection [1 5] [5 6]) [5 5])) ;; Partial overlap on the right edge
  (is (= (range-intersection [1 5] [2 3]) [2 3])) ;; Partial overlap in the middle
  )

(def final-ranges
  (loop [parsed parsed
         ranges []]
    ;; (prn  ranges)
    (prn (count-ons ranges))
    (prn (first parsed))
    (if (empty? parsed)
      ranges
      (let [[cmd range-3d] (first parsed)
            [[fx1 tox1] [fy1 toy1] [fz1 toz1]] range-3d]
        (if (or (< tox1 -50) (> fx1 50)
                (< toy1 -50) (> fy1 50)
                (< toz1 -50) (> fz1 50))
          (recur (rest parsed) ranges)
          (let [new-ranges (if (= cmd "on")
                             (add-ranges-3d ranges [range-3d])
                             (subtract-ranges-3d ranges [range-3d]))]
            (recur (rest parsed) new-ranges)))))))

;; (def final-ranges
;;   (loop [parsed parsed
;;          ranges []]
;;     ;; (prn  ranges)
;;     (if (empty? parsed)
;;       ranges
;;       (let [[cmd range-3d] (first parsed)
;;             new-ranges (if (= cmd "on")
;;                          (add-ranges-3d ranges [range-3d])
;;                          (subtract-ranges-3d ranges [range-3d]))]
;;         (recur (rest parsed) new-ranges)))))





;; 1208337840132962 too low


;; [[fx1 tox1] [fy1 toy1] [fz1 toz1]]

;; (def answer-2 nil)
;; ;; Execute the reboot steps. 
;; ;; Afterward, considering only cubes in the region x=-50..50,y=-50..50,z=-50..50, 
;; ;; how many cubes are on?

;; (defn- -main [& _]
;;   (println "Day XX, Part 1:" answer-1)
;;   (println "Day XX, Part 2:" answer-2))

;; (-main)
