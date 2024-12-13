(ns advent.2024.d13.core
  (:require
   [advent.util :refer [str->nums]]
   [clojure.string :as str]))

(def input (slurp "src/advent/2024/d13/input.txt"))

;; SECTIONS
(defn parse-section
  [section]
  (let [[a b p] (str/split-lines section)]
    {:A (str->nums a)
     :B (str->nums b)
     :prize (str->nums p)}))

(defn parse-sections [input]
  (map parse-section (str/split input #"\n\n")))

(def claws (parse-sections input))
;; 

(defn move-claw
  [claw timesA timesB]
  [(+ (* timesA (get-in claw [:A 0]))
      (* timesB (get-in claw [:B 0])))
   (+ (* timesA (get-in claw [:A 1]))
      (* timesB (get-in claw [:B 1])))
   (+ (* timesA 3) (* timesB 1))])

;; brute force solution
;; works only for part 1
(defn win-claw [claw]
  (first (for [timesA (range 101)
               timesB (range 101)
               :let [[x y cost] (move-claw claw timesA timesB)]
               :when (and (= x (get-in claw [:prize 0]))
                          (= y (get-in claw [:prize 1])))]

           [timesA timesB cost])))

;; What is the fewest tokens you would have to spend to win all possible prizes?
;; (def answer-1 (apply + (remove nil? (map #(get % 2) (mapv win-claw claws)))))

;; PART 2

(def claws-2
  (mapv (fn [claw]
          (assoc claw :prize (mapv #(+ %  10000000000000) (:prize claw))))
        claws))

(defn inverse-transform
  "Transforms a point [px py] from the custom coordinate system
   to the standard Cartesian coordinate system using the basis vectors vx and vy."
  [point vx vy]
  (let [[px py] point
        [vx1 vx2] vx
        [vy1 vy2] vy
        det (- (* vx1 vy2) (* vx2 vy1))] ;; Determinant of the transformation matrix
    (if (zero? det)
      (throw (ex-info "Transformation matrix is singular and cannot be inverted." {}))
      (let [inv-det (/ 1.0 det) ;; Inverse of the determinant
            inv-matrix [[(* inv-det vy2) (* inv-det (- vy1))]
                        [(* inv-det (- vx2)) (* inv-det vx1)]]
            [[a b] [c d]] inv-matrix]
        [(+ (* px a) (* py b))
         (+ (* px c) (* py d))]))))

(defn close-to-integer?
  "Checks if a number n is close to an integer within a given threshold (default 1e-9)."
  [n] (< (Math/abs (- n (Math/round n))) 1e-3))

(defn win-claw-2
  [claw]
  (let [vx (:A claw)
        vy (:B claw)
        prize (:prize claw)
        [tA tB] (inverse-transform prize vx vy)]
    (if (and (close-to-integer? tA)
             (close-to-integer? tB))
      (let [tA (Math/round tA)
            tB (Math/round tB)
            prize-value (+ (* tA 3) (* tB 1))]
        [tA tB prize-value])
      nil)))

(defn answer
  [claws]
  (apply + (remove nil? (map #(get % 2)
                             (mapv win-claw-2 claws)))))

(defn- -main [& _]
  (println "Day 13, Part 1:" (answer claws))
  (println "Day 13, Part 2:" (answer claws-2)))

(-main)

