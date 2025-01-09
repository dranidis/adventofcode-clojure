(ns advent.2021.d22.core
  (:require
   [advent.util :refer [parse-lines-with-parser str->nums]]
   [clojure.test :refer [is testing]]))

(defn line-parser [line]
  (let [[[_ state xys]] (re-seq #"(on|off)(.*)" line)]
    [state (mapv vec (partition 2 (str->nums xys)))]))

(def parsed (parse-lines-with-parser line-parser (slurp "src/advent/2021/d22/input.txt")))

(defn on?
  [x y z times]
  (loop [parsed parsed
         state 0
         t 0]
    (if (or (= t times) (empty? parsed))
      state
      (let [[cmd [[fx tox] [fy toy] [fz toz]]] (first parsed)
            new-state (if (and (<= fx x tox)
                               (<= fy y toy)
                               (<= fz z toz))
                        (if (= cmd "on") 1 0)
                        state)]
        (recur (rest parsed) new-state (inc t))))))


;; Brute force
(defn answer-1 [] (count (for [x (range -50 51)
                               y (range -50 51)
                               z (range -50 51)
                               :when (= 1 (on? x y z 20))]
                           [x y z])))

(println (answer-1))

(defn range-intersection
  "Return the intersection of two ranges."
  [[fx1 tox1] [fx2 tox2]]
  (let [sorted (sort [[fx1 tox1] [fx2 tox2]])
        [[fx1 tox1] [fx2 tox2]] sorted]
    (if (> fx2 tox1)
      nil
      [(max fx1 fx2) (min tox1 tox2)])))

(comment
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
    ))

(defn range-intersection-3d
  [range1 range2]
  (let [[x1 x2 x3] range1
        [y1 y2 y3] range2
        x (range-intersection x1 y1)
        y (range-intersection x2 y2)
        z (range-intersection x3 y3)]
    (if (or (nil? x) (nil? y) (nil? z))
      []
      [x y z])))

(defn sz [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))

(defn sim-ranges []
  (loop [parsed parsed
         ons []
         offs []
         cnt 0]
    (if (empty? parsed)
      cnt
      (let [[cmd rng] (first parsed)
            intsections-with-ons (->> ons
                                      (mapv (fn [o] (range-intersection-3d rng o)))
                                      (remove empty?))
            inssections-with-offs (->> offs
                                       (mapv (fn [o] (range-intersection-3d rng o)))
                                       (remove empty?))
            new-offs (apply conj offs intsections-with-ons)
            new-ons (apply conj ons inssections-with-offs)
            new-ons (if  (= cmd "on") (conj new-ons rng) new-ons)
            new-cnt (- (+ cnt (if (= cmd "on") (sz rng) 0)
                          (apply + (map sz inssections-with-offs)))
                       (apply + (map sz intsections-with-ons)))]
        (recur (rest parsed)
               new-ons
               new-offs
               new-cnt)))))

(println "ANS 2: " (sim-ranges))
