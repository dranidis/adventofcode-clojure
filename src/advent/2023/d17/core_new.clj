(ns advent.2023.d17.core-new
  (:require
   [advent.2023.d17.priority-queue :refer [add-with-priority! extract-min!
                                           make-priority-queue!]]
   [advent.util :refer [str->2D-num]]))

(def input (slurp "src/advent/2023/d17/input.txt"))

(def input "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
")

(defn next-positions [the-map num-rows num-cols r c dist dir indir]
  (for [[new-dir [dr dc]] (map-indexed vector
                                       [[-1 0] [0 1] [1 0] [0 -1]])
        :let [rr (+ r dr)
              cc (+ c dc)
              new-indir (if (= new-dir dir) (inc indir) 1)]
        :when (and (<= 0 rr (dec num-rows))
                   (<= 0 cc (dec num-cols))
                   (<= new-indir 3)
                   (not= dir (mod (+ 2 new-dir) 4)))]
    [(+ dist (get-in the-map [rr cc])) rr cc new-dir new-indir]))

(defn answer1 [input next-positions]
  (let [the-map (str->2D-num input)
        num-cols (count (first the-map))
        num-rows (count the-map)
        ;; Q (conj (PersistentQueue/EMPTY) [0 0 0 -1 0])
        Q (add-with-priority! (make-priority-queue!) [0 0 0 -1 0])
        D {}]
    (loop [Q Q
           D D]
      ;; (println D)
      (if (not (empty? Q))
        (let [[dist r c dir indir] (extract-min! Q)
              ;; Q (pop Q)
              ]
          ;; (printf "r=%d c=%d dist=%d dir=%d indir=%d", r, c, dist, dir, indir)
          ;; (println)
          (if (get D [r c dir indir])
            (recur Q D)
            (let [D (assoc D [r c dir indir] dist)
                  Q (apply add-with-priority! Q
                           (next-positions the-map num-rows num-cols r c dist dir indir))]
              (recur Q D))))
        (apply min (for [[[r c dir indir] v] (vec D)
                         :when (and (= r (dec num-rows))
                                    (= c (dec num-cols)))]
                     v))))))

(defn next-positions-2 [the-map num-rows num-cols r c dist dir indir]
  (for [[new-dir [dr dc]] (map-indexed vector
                                       [[-1 0] [0 1] [1 0] [0 -1]])
        :let [rr (+ r dr)
              cc (+ c dc)
              new-indir (if (= new-dir dir) (inc indir) 1)]
        :when (and (<= 0 rr (dec num-rows))
                   (<= 0 cc (dec num-cols))
                   (<= new-indir 10)
                   (or (= dir -1) (>= indir 4) (= new-dir dir))
                   (not= dir (mod (+ 2 new-dir) 4)))]
    [(+ dist (get-in the-map [rr cc])) rr cc new-dir new-indir]))


;;  1135
(defn -main [& _]
  (let [input
        input
        ;; (slurp "src/advent/2023/d17/input.txt")
        ]
    (time (println "Day 1, Part 1:" (answer1 input next-positions)))
    (time (println "Day 1, Part 2:" (answer1 input next-positions-2)))))