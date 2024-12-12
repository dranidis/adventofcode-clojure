(ns advent.2021.d19.core
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.test :refer [is]] ;;  [clojure.walk :refer [postwalk prewalk]]

   [clojure.set :as set]))

(def example? true)

(def input (if example?
             (slurp "src/advent/2021/d19/example.txt")
             (slurp "src/advent/2021/d19/input.txt")))

(defn- parse-scanner
  [section]
  (vec (for [line (rest (str/split-lines section))]
         (mapv parse-long (re-seq #"-*\d+" line)))))

(defn parse-parts
  [input]
  (let [sections (str/split input #"\n\n")]
    (mapv parse-scanner sections)))

;; 

(def beacons (parse-parts input))
(def scanners (count beacons))

(defn distance
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (* (- x2 x1) (- x2 x1))
     (* (- y2 y1) (- y2 y1))
     (* (- z2 z1) (- z2 z1))))

(def info (for [scanner1 (range scanners)
                scanner2 (range scanners)
                :when (> scanner1 scanner2)
                :let [distances1 (set (for [b1 (beacons scanner1)
                                            b2 (beacons scanner1)
                                            :when (not= b1 b2)]
                                        (distance b1 b2)))
                      distances2 (set (for [b1 (beacons scanner2)
                                            b2 (beacons scanner2)
                                            :when (not= b1 b2)]
                                        (distance b1 b2)))
                      common-distances (set/intersection distances1 distances2)]
                :let [cnt (count common-distances)]
                :when (>= cnt 66)]
            {:scanner1 scanner1 :scanner2 scanner2
             :count cnt :beacon-dists common-distances}))

(def pairs (let [s (second info)
                 s1 (:scanner1 s)
                 s2 (:scanner2 s)]
             (for [s1b1 (beacons s1)
                   s1b2 (beacons s1)
                   s2b1 (beacons s2)
                   s2b2 (beacons s2)
                   :let [d1 (distance s1b1 s1b2)
                         d2 (distance s2b1 s2b2)]
                   :when (= d1 d2 (first (:beacon-dists s)))]
               [s1b1 s1b2 s2b1 s2b2])))

(def rotations [[[1 0 0] [0 1 0] [0 0 1]]
                [[1 0 0] [0 0 1] [0 -1 0]]
                [[1 0 0] [0 -1 0] [0 0 -1]]
                [[1 0 0] [0 0 -1] [0 1 0]]
                [[0 1 0] [1 0 0] [0 0 1]]
                [[0 1 0] [0 0 1] [-1 0 0]]
                [[0 1 0] [-1 0 0] [0 0 -1]]
                [[0 1 0] [0 0 -1] [1 0 0]]
                [[0 0 1] [1 0 0] [0 1 0]]
                [[0 0 1] [0 1 0] [-1 0 0]]
                [[0 0 1] [-1 0 0] [0 -1 0]]
                [[0 0 1] [0 -1 0] [1 0 0]]
                [[-1 0 0] [0 -1 0] [0 0 1]]
                [[-1 0 0] [0 0 1] [0 1 0]]
                [[-1 0 0] [0 1 0] [0 0 -1]]
                [[-1 0 0] [0 0 -1] [0 -1 0]]
                [[0 -1 0] [1 0 0] [0 0 -1]]
                [[0 -1 0] [0 0 -1] [-1 0 0]]
                [[0 -1 0] [-1 0 0] [0 0 1]]
                [[0 -1 0] [0 0 1] [1 0 0]]
                [[0 0 -1] [1 0 0] [0 -1 0]]
                [[0 0 -1] [0 -1 0] [-1 0 0]]
                [[0 0 -1] [-1 0 0] [0 1 0]]
                [[0 0 -1] [0 1 0] [1 0 0]]])

(defn dot-product
  "Calculate the dot product of two vectors."
  [v1 v2]
  (reduce + (map * v1 v2)))

(defn multiply-matrix-vector
  "Multiply a 3x3 matrix by a 3D vector."
  [matrix vector]
  (mapv #(dot-product % vector) matrix))

(multiply-matrix-vector (last rotations) [1 2 3])

(defn cross-product
  "Calculate the cross product of two 3D vectors."
  [[x1 y1 z1] [x2 y2 z2]]
  [(- (* y1 z2) (* z1 y2))
   (- (* z1 x2) (* x1 z2))
   (- (* x1 y2) (* y1 x2))])

(defn zero-vector?
  "Check if a vector is the zero vector."
  [v]
  (every? zero? v))

(defn parallel?
  "Check if two 3D vectors are parallel."
  [v1 v2]
  (zero-vector? (cross-product v1 v2)))




(for [[s1b1 s1b2 s2b1 s2b2] (for [s [(first info)]
                                  bd (:beacon-dists s)
                                  :let [s1 (:scanner1 s)
                                        s2 (:scanner2 s)]
                                  s1b1 (beacons s1)
                                  s1b2 (beacons s1)
                                  s2b1 (beacons s2)
                                  s2b2 (beacons s2)
                                  :let [d1 (distance s1b1 s1b2)
                                        d2 (distance s2b1 s2b2)]
                                  :when (= d1 d2 bd)]
                              [s1b1 s1b2 s2b1 s2b2])
      r rotations
      :when (parallel? (multiply-matrix-vector r s1b1) s2b1)]
  s1b1)





(clojure.pprint/pp)
(comment




  (multiply-matrix-vector (first rotations) [1 2 3])

  (for [b [[1 2 3]]
        r rotations]
    (multiply-matrix-vector r b))

;; Assemble the full map of beacons. How many beacons are there?

  (defn translate
    [b xt yt zt]
    (mapv (partial mapv + [xt yt zt]) (beacons b)))

  (defn change-axis
    [[x y z]]
    [[x y z] [y z x] [z x y] [x z y] [z y x] [y x z]])

  (comment
    (set (beacons 0))
    (set (translate 0 1 1 1))
    (set/intersection #{[1 1 1]} #{[1 1 1] [1 2 3]})
;
    )
;; (set/intersection (set (beacons 0)) (set (translate 1 68 -1246 -43)))

;; (set/intersection (set (beacons 0)) (set (translate 1 -68 -1246 -43)))
;; (set/intersection (set (beacons 0)) (set (translate 1 68 1246 -43)))
;; (set/intersection (set (beacons 0)) (set (translate 1 68 -1246 43)))

;; (set/intersection (set (beacons 0)) (set (translate 1 -68 +1246 -43)))
;; (set/intersection (set (beacons 0)) (set (translate 1 -68 +1246 -43)))

  (def tv [68 -1246 -43])
  (def t [1 1 1])
  (mapv * t tv)

  (for [tv (change-axis [68,-1246,-43])
        t [[1 1 1] [1 1 -1] [1 -1 1] [1 -1 -1]
           [-1 1 1] [-1 1 -1] [-1 -1 1] [-1 -1 -1]]
        [b1 b2] [[0 1] [1 0]]
        :let [[tx ty tz] (mapv * t tv)]]
    (set/intersection (set (beacons b1)) (set (translate b2 tx ty tz))))

  (clojure.pprint/pp)

  (for [tv (change-axis [68,-1246,-43])
        t [[1 1 1] [1 1 -1] [1 -1 1] [1 -1 -1]
           [-1 1 1] [-1 1 -1] [-1 -1 1] [-1 -1 -1]]
        [b1 b2] [[0 1] [1 0]]
        :let [[tx ty tz] (mapv * t tv)]]
    (set/intersection (set (beacons b1)) (set (translate b2 tx ty tz))))

  (for [tv (change-axis [68,-1246,-43])
        t [[1 1 1] [1 1 -1] [1 -1 1] [1 -1 -1]
           [-1 1 1] [-1 1 -1] [-1 -1 1] [-1 -1 -1]]]
    (map (fn  [v] (mapv + (map * t tv) v)) (parse-scanner "-618,-824,-621
-537,-823,-458
-447,-329,318
404,-588,-901
544,-627,-890
528,-643,409
-661,-816,-575
390,-675,-793
423,-701,434
-345,-311,381
459,-707,401
-485,-357,347")))

  (beacons 1)
;
  )