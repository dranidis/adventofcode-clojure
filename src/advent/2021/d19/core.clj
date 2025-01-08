(ns advent.2021.d19.core
  (:require
   [advent.util :refer [manhattan-distance-3d]]
   [clojure.set :as set]
   [clojure.string :as str]))

(def example? false)

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

(def beacons (parse-parts input))
(def scanners-num (count beacons))

(defn euclidian-3d [[x1 y1 z1] [x2 y2 z2]]
  (let [sqr (fn [x] (* x x))]
    (+ (sqr (- x2 x1))
       (sqr (- y2 y1))
       (sqr (- z2 z1)))))

(def info (for [scanner1 (range scanners-num)
                scanner2 (range scanners-num)
                :when (< scanner1 scanner2)
                :let [distances1 (set (for [b1 (beacons scanner1)
                                            b2 (beacons scanner1)
                                            :when (not= b1 b2)]
                                        (euclidian-3d b1 b2)))
                      distances2 (set (for [b1 (beacons scanner2)
                                            b2 (beacons scanner2)
                                            :when (not= b1 b2)]
                                        (euclidian-3d b1 b2)))
                      common-distances (set/intersection distances1 distances2)]
                :let [cnt (count common-distances)]
                :when (>= cnt 66)]
            {:scanner1 scanner1 :scanner2 scanner2
             :count cnt :beacon-dists common-distances}))

(defn multiply-matrix-vector
  "Multiply a 3x3 matrix by a 3D vector."
  [matrix vector]
  (let [dot-product
        ;; "Calculate the dot product of two vectors."
        (fn [v1 v2]
          (reduce + (mapv * v1 v2)))]
    (mapv #(dot-product % vector) matrix)))

;; https://www.euclideanspace.com/maths/discrete/groups/categorise/finite/cube/index.htm
(def rotations [[[1 0 0] [0 1 0] [0 0 1]]
                [[1 0 0] [0 0 -1] [0 1 0]]
                [[1 0 0] [0 -1 0] [0 0 -1]]
                [[1 0 0] [0 0 1] [0 -1 0]]
                [[0 -1 0] [1 0 0] [0 0 1]]
                [[0 0 1] [1 0 0] [0 1 0]]
                [[0 1 0] [1 0 0] [0 0 -1]]
                [[0 0 -1] [1 0 0] [0 -1 0]]
                [[-1 0 0] [0 -1 0] [0 0 1]]
                [[-1 0 0] [0 0 -1] [0 -1 0]]
                [[-1 0 0] [0 1 0] [0 0 -1]]
                [[-1 0 0] [0 0 1] [0 1 0]]
                [[0 1 0] [-1 0 0] [0 0 1]]
                [[0 0 1] [-1 0 0] [0 -1 0]]
                [[0 -1 0] [-1 0 0] [0 0 -1]]
                [[0 0 -1] [-1 0 0] [0 1 0]]
                [[0 0 -1] [0 1 0] [1 0 0]]
                [[0 1 0] [0 0 1] [1 0 0]]
                [[0 0 1] [0 -1 0] [1 0 0]]
                [[0 -1 0] [0 0 -1] [1 0 0]]
                [[0 0 -1] [0 -1 0] [-1 0 0]]
                [[0 -1 0] [0 0 1] [-1 0 0]]
                [[0 0 1] [0 1 0] [-1 0 0]]
                [[0 1 0] [0 0 -1] [-1 0 0]]])

;; If I find the matching point pairs, then I can find the relative distance and rotation 
(defn tr-m [rot-2D vec1 vec2]
  (let [[trans-point-rel-to-1 rotation-matrix]
        (first (for [r rot-2D
                     :let [trans (mapv (fn [v1 v2] (mapv - v1 (multiply-matrix-vector r v2)))
                                       vec1
                                       vec2)]
                     :when (= 1 (->> trans set count))]
                 [(first trans) r]))]
    [trans-point-rel-to-1 rotation-matrix]))

(defn tranform-rel-to-1 [[trans-point-rel-to-1 rotation-matrix] pos-rel-2]
  (mapv + trans-point-rel-to-1 (multiply-matrix-vector rotation-matrix pos-rel-2)))

(defn pairs-2d [s1 s2]
  (for [p11 s1]
    (first (for [p12 s1
                 :when (not= p11 p12)]
             ;; find triangles with equal distances
             (first (for [p13 s1
                          :when (and (not= p13 p12)
                                     (not= p13 p11))
                          p21 s2
                          p22 s2
                          :when (not= p22 p21)
                          p23 s2
                          :when (and (not= p23 p22)
                                     (not= p23 p21))
                          :let [d1-12 (manhattan-distance-3d p11 p12)
                                d1-13 (manhattan-distance-3d p11 p13)
                                d1-23 (manhattan-distance-3d p12 p13)
                                d2-12 (manhattan-distance-3d p21 p22)
                                d2-13 (manhattan-distance-3d p21 p23)
                                d2-23 (manhattan-distance-3d p22 p23)]
                          :when (and (= d1-12 d2-12)
                                     (= d1-13 d2-13)
                                     (= d1-23 d2-23))]
                      [p11 p21]))))))

(defn beacons-detected-by-both-0-and-1-relative-to-0 [info-part trans1 trans2]
  (let [s1 (:scanner1 info-part)
        bs1 (if (some? trans1)
              (mapv (partial tranform-rel-to-1 trans1) (beacons s1))
              (beacons s1))
        s2 (:scanner2 info-part)
        bs2 (if (some? trans2)
              (mapv (partial tranform-rel-to-1 trans2) (beacons s2))
              (beacons s2))]
    (set (for [s1b1 bs1
               s1b2 bs1
               s2b1 bs2
               s2b2 bs2
               :let [d1 (euclidian-3d s1b1 s1b2)
                     d2 (euclidian-3d s2b1 s2b2)]
               :when (= d1 d2)
               :when ((:beacon-dists info-part) d1)]
           s1b1))))

(defn beacons-detected-by-both-0-and-1-relative-to-1 [info-part trans1 trans2]
  (let [s1 (:scanner1 info-part)
        bs1 (if (some? trans1)
              (mapv (partial tranform-rel-to-1 trans1) (beacons s1))
              (beacons s1))
        s2 (:scanner2 info-part)
        bs2 (if (some? trans2)
              (mapv (partial tranform-rel-to-1 trans2) (beacons s2))
              (beacons s2))]
    (set (for [s1b1 bs1
               s1b2 bs1
               s2b1 bs2
               s2b2 bs2
               :let [d1 (euclidian-3d s1b1 s1b2)
                     d2 (euclidian-3d s2b1 s2b2)]
               :when (= d1 d2)
               :when ((:beacon-dists info-part) d1)]
           s2b1))))

(def transformations
  (loop [info info
         pending []
         Dtr {0 nil}]
    ;; (println Dtr)
    (if (empty? info)
      (if (empty? pending)
        Dtr
        (recur pending [] Dtr))
      (let [fi (first info)
            mapped-fi? (contains? Dtr (:scanner1 fi))]
        (if (or mapped-fi? (contains? Dtr (:scanner2 fi)))
          (let [[trans1 trans2] (if mapped-fi?
                                  [(Dtr (:scanner1 fi)) nil]
                                  [nil (Dtr (:scanner2 fi))])
                p-2d (pairs-2d (beacons-detected-by-both-0-and-1-relative-to-0 fi trans1 trans2)
                               (beacons-detected-by-both-0-and-1-relative-to-1 fi trans1 trans2))
                v1 (mapv first p-2d)
                v2 (mapv second p-2d)
                tr01 (if mapped-fi?
                       (tr-m rotations v1 v2)
                       (tr-m rotations v2 v1))
                Dtr (assoc Dtr (if mapped-fi?
                                 (:scanner2 fi)
                                 (:scanner1 fi))
                           tr01)]
            (recur (rest info) pending Dtr))
          (recur (rest info) (conj pending fi) Dtr))))))


(def all-beacons (->> transformations
                      (reduce-kv (fn [m k v]
                                   (let [b (if (some? v)
                                             (mapv (partial tranform-rel-to-1 v) (beacons k))
                                             (beacons k))]
                                     (apply conj m b)))
                                 #{})
                      vec))

(println "ANS 1" (count all-beacons))


(def all-scanners (conj (remove nil? (map first (vals transformations))) [0 0 0]))

(println "ANS 2" (->> (for [s1 all-scanners
                            s2 all-scanners]
                        (manhattan-distance-3d s1 s2))
                      (apply max)))

