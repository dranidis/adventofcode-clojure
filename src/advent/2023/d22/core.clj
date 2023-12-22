(ns advent.2023.d22.core
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.set :as set]))

(def input "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn parse
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (let [nums (re-seq #"\d+" line)]
                (map vec (partition 3 (mapv parse-long nums))))))))

(comment
  (sorted-bricks (parse input))
  ;
  )

(defn gx [[x y z]] x)
(defn gy [[x y z]] y)
(defn gz [[x y z]] z)

(defn sorted-bricks [pi] (sort (fn [[a1 b1] [a2 b2]]
                                 (< (gz a1) (gz a2)))
                               pi))

(defn top-brick-coords [brick]
  (for [x (range (gx (first brick)) (inc (gx (second brick))))
        y (range (gy (first brick)) (inc (gy (second brick))))]
    [x y (gz (second brick))]))

(defn bottom-brick-coords [brick]
  (for [x (range (gx (first brick)) (inc (gx (second brick))))
        y (range (gy (first brick)) (inc (gy (second brick))))]
    [x y (gz (first brick))]))

(defn debug [msg v]
  (println msg v)
  v)

(defn supports? [A B]
  (some true? (for [[ax ay az] (top-brick-coords A)
                    [bx by bz] (bottom-brick-coords B)]
                (and (= ax bx)
                     (= ay by)
                     (= az (dec bz))))))

(defn my-max [coll]
  (if (empty? coll) 0 (apply max coll)))

(defn falls-until-supported [bricks-below brick]
  (let [[[bx1 by1 bz1] [bx2 by2 bz2]] brick
        max-brick-z (my-max
                     (map gz (filter (fn [[x y z]]
                                       (and (<= bx1 x bx2)
                                            (<= by1 y by2)))
                                     (apply concat
                                            (map top-brick-coords
                                                 bricks-below)))))
        diff-z (- bz2 bz1)
        new-z1 (inc max-brick-z)
        new-z2 (+ new-z1 diff-z)]
    [[bx1 by1 new-z1] [bx2 by2 new-z2]]))

(defn fallen-bricks [pi]
  (loop [bricks (sorted-bricks pi)
         tower #{}
         fallen []
         t 0]
    ;; (println t)
    (if (empty? bricks)
      fallen
      (let [brick (first bricks)
            next-bricks (next bricks)
            new-brick (falls-until-supported fallen brick)
            fallen (conj fallen new-brick)]
        (recur next-bricks tower fallen (inc t))))))

(defn collect-supported-by [bricks]
  (zipmap bricks
          (for [[[bx1 by1 bz1] [bx2 by2 bz2]] bricks]
            (filter (fn [[[x1 y1 z1] [x2 y2 z2]]]
                      (and (= (inc z2) bz1)
                           (seq (set/intersection (set (bottom-brick-coords [[bx1 by1 (dec bz1)] [bx2 by2 (dec bz2)]]))
                                                  (set (top-brick-coords [[x1 y1 z1] [x2 y2 z2]]))))))
                    bricks))))

(defn collect-supporting [bricks]
  (zipmap bricks
          (for [[[bx1 by1 bz1] [bx2 by2 bz2]] bricks]
            (filter (fn [[[x1 y1 z1] [x2 y2 z2]]]
                      (and (= (inc bz2) z1)
                           (seq (set/intersection (set (top-brick-coords [[bx1 by1 (inc bz1)] [bx2 by2 (inc bz2)]]))
                                                  (set (bottom-brick-coords [[x1 y1 z1] [x2 y2 z2]]))))))
                    bricks))))

(defn can-be-removed? [supporting supported-by brick]
  (or (empty? (get supporting brick))
      (every? (fn [b] (> (count (get supported-by b)) 1)) (get supporting brick))))

(defn all-that-can-be-removed [pi]
  (let [bricks (sorted-bricks (fallen-bricks pi))
        supported-by (collect-supported-by bricks)
        supporting (collect-supporting bricks)]
    (for [brick bricks
          :when (can-be-removed? supporting supported-by brick)]
      brick)))

(defn answer-1 [input]
  (count (all-that-can-be-removed (parse input))))

(defn bricks-falling [supported-by brick-to-remove]
  (loop [D supported-by
         brick-to-remove brick-to-remove
         t 0]
    (if (nil? brick-to-remove)
      (dec t)
      (let [D (reduce
               (fn [m [k v]]
                 (if (= k brick-to-remove)
                   m
                   (assoc m k
                          (remove (fn [b] (= brick-to-remove b)) v))))
               {}
               D)
            keys-to-remove (reduce (fn [m [k v]]
                                     (if (and (> (gz (first k)) 1)
                                              (empty? v))
                                       (conj m k)
                                       m))
                                   []
                                   D)]
        (recur D (first keys-to-remove) (inc t))))))

(defn answer-2 [input]
  (let [pi (parse input)
        bricks (sorted-bricks (fallen-bricks pi))
        supported-by (collect-supported-by bricks)
        supporting (collect-supporting bricks)]
    (apply +
           (map (partial bricks-falling supported-by)
                (remove (fn [b] (can-be-removed? supporting supported-by b))
                        bricks)))))

(defn -main [& _]
  (time (println "Day 22 part 1" (answer-1 (slurp "src/advent/2023/d22/input.txt"))))
  (time (println "Day 22 part 2" (answer-2 (slurp "src/advent/2023/d22/input.txt"))))

  ; 
  )