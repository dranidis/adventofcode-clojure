(ns advent.2024.d20.core
  (:require
   [advent.2023.d17.priority-queue :refer [add-with-priority! extract-min!
                                           make-priority-queue!]]
   [advent.util :refer [coords-of-symbol in-vector? str->2D]]
   [clojure.core.reducers :as reducers]
   [clojure.test :refer [is]]))

(def example? false)

(def example "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(def input (if example? example (slurp "src/advent/2024/d20/input.txt")))

;; GRID
;; (def grid (str->2D-num input))
(def grid (str->2D input))
(def rows (count grid))
(def cols (count (first grid)))

(def inside-walls (set (for [w (coords-of-symbol grid "#")
                             :let [[wr wc] w]
                             :when (or (not= wr 0)
                                       (not= wc 0)
                                       (not= wr (dec rows))
                                       (not= wc (dec cols)))]
                         w)))

(def start (first (coords-of-symbol grid "S")))
(def end (first (coords-of-symbol grid "E")))

(defn dijkstra-shortest-distance-to-end-with-d-1-and-cutoff
  "Return the shortest distances from start to end.
   - start: node 
   - neighbors: node -> [neighbor]  ;; all neighbors have distance 1
   - returns: distance or nil if the distance is greater than cutoff."
  [start end neighbors cutoff]
  ;; (println "Dijkstra with cutoff" cutoff)
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2)))
                              [0 start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist node] (extract-min! Q)]
          (if (> dist cutoff)
            nil
            (if (get D node)
              (recur Q D)
              (let [nexts (vec (neighbors node))]
                (if (in-vector? nexts end)
                  (inc dist)
                  (let [D (assoc D node dist)
                        Q (apply add-with-priority! Q
                                 (map (fn [n]
                                        [(inc dist) n])
                                      nexts))]
                    (recur Q D)))))))
        nil))))


(defn dijkstra-shortest-distance-to-end-with-d-1-and-cutoff
  "Return the shortest distances from start to end.
   - start: node 
   - neighbors: node -> [neighbor]  ;; all neighbors have distance 1
   - returns: distance or nil if the distance is greater than cutoff."
  [start end neighbors cutoff]
  ;; (println "Dijkstra with cutoff" cutoff)
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2)))
                              [0 start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist node] (extract-min! Q)]
          (if (= node end)
            dist
            (if (get D node)
              (recur Q D)
              (let [nexts (vec (neighbors node))]
                (if (in-vector? nexts end)
                  (inc dist)
                  (let [D (assoc D node dist)
                        Q (apply add-with-priority! Q
                                 (map (fn [n]
                                        [(inc dist) n])
                                      nexts))]
                    (recur Q D)))))))
        nil))))

(defn next-w-fn
  [walls]
  (fn [[r c]]
    (vec (for [[dr dc] [[1 0] [0 1] [-1 0] [0 -1]]
               :let [nr (+ r dr)
                     nc (+ c dc)]
               :when (and (< 0 nr (dec rows))
                          (< 0 nc (dec cols)))
               :when (not (walls [nr nc]))]
           [nr nc]))))

(defn walls-at-point
  [p]
  (let [[r c] p]
    (for [[dr dc] [[0 1] [1 0] [0 -1] [-1 0]]
          :let [nr (+ dr r)
                nc (+ dc c)]
          :when (and (inside-walls [nr nc])
                     (not (inside-walls [(+ nr dr) (+ nc dc)])))]
      [nr nc])))

(defn single-path-to-end
  [start end inside-walls]
  (loop [start start
         path [start]
         visited #{start}]
    (if (= start end)
      path
      (let [ns ((next-w-fn inside-walls) start)
            ns (remove visited ns)
            _ (assert (= 1 (count ns))
                      (str "Only one next allowed"
                           start path ns))
            n (first ns)
            visited (conj visited n)]
        (recur n (conj path n) visited)))))

(defn- try-diff-walls
  "Try a path by setting a wall at point n and removing a wall at w"
  [start n w cutoff]
  (let [inside-walls (disj (conj inside-walls n) w)]
    (dijkstra-shortest-distance-to-end-with-d-1-and-cutoff
     start end (next-w-fn inside-walls) cutoff)))

(defn savings-of-alt-walls
  [start end inside-walls]
  (let [main-path (single-path-to-end start end inside-walls)
        main-path-len (count main-path)]
    (loop [path main-path
           saving-list []
           path-len main-path-len]
      (if (= 1 (count path)) ;; reached the end point
        saving-list
        (let [start (first path)
              n (second path)

              walls (walls-at-point start)
              savings (pmap (fn [w]
                              (if-let [l (try-diff-walls start n w path-len)]
                                (dec (- path-len l))
                                0))
                            walls)]
          (recur (rest path)
                 (conj saving-list savings)
                 (dec path-len)))))))

(defn answer-1
  []
  (let [savings (flatten (savings-of-alt-walls start end inside-walls))]
    (println (count (filter (fn [s] (>= s (if example? 64 100)))
                            savings)))))

;; (println (answer-1))

(when example?
  (let [savings (flatten (savings-of-alt-walls start end inside-walls))
        f (frequencies (remove zero? savings))]
    (is (= f
           (frequencies (remove zero? '(38 64 40 0 0 0 36 2 0 4 0 2 0 8 2 10 0 0 0 0 0 0 0 0 2 12 0 0 0 0 0 6 0 2 0 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 4 0 4 0 0 0 4 6 4 0 0 2 4 0 12 2 0 0 2 0 0 0 0 4 0 0 4 4 0 0 0 2 4 0 2 0 2 4 0 20 0 0 0 0 0 0 0 0 0 0 0 8 0 0 0 0 8 0 0 0 0 2 4 0 0 0 0 2 0 0 0 12 0 2 4 0 0 0 8 0 0 4 0 0 0 0)))
                   ;
           ))
    (is (= 1 (f 64)))
    (is (= 1 (f 40)))
    (is (= 1 (f 38)))
    (is (= 1 (f 36)))
    (is (= 1 (f 20)))
    (is (= 3 (f 12)))
      ;
    ))


(defn dijkstra-shortest-distances
  "Return the shortest distances from start to all other nodes.
   
   - start: node 
   - neighbors: node -> [[distance neighbor]]
   - returns: {detination-node distance}

   The neighbors is a function mapping a node to its 
   neighbors, which are a list of [distance neighbor] pairs.
   The result is a map from destination node to distance from the start."
  [start neighbors]
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2)))
                              [0 start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist node] (extract-min! Q)]
      ;;     (println "Current node" node "distance" dist "queue" Q)
      ;;     (println "Distances" D)
          (if (get D node)
            (recur Q D)
            (let [D (assoc D node dist)
                  Q (apply add-with-priority! Q
                           (map (fn [[d n]]
                                  ;; (println "Adding" n "with distance" (+ dist d))
                                  [(+ dist d) n])
                                (neighbors node)))]
              (recur Q D))))
        D))))

(defn next-dist
  [[r c]]
  (mapv (fn [n] [1 n]) ((next-w-fn inside-walls) [r c])))

(def from-start (dijkstra-shortest-distances start next-dist))
(def start-keys (keys from-start))
(println (count start-keys))

(def from-end (dijkstra-shortest-distances end next-dist))
(def end-keys (keys from-end))
(println (count end-keys))

(defn manhattan-distance
  [[r1 c1] [r2 c2]]
  (+ (Math/abs (- r1 r2))
     (Math/abs (- c1 c2))))

(def main-path-len (count (single-path-to-end start end inside-walls)))

(defn x
  [num]
  (count (for [s start-keys
               e end-keys
               :let [dist (manhattan-distance s e)]
               :when (<= dist num)
               :let [total (+ (from-start s) (from-end e) dist)]
               :when (>= (- main-path-len total) (if example? 76 100))]
           [s e])))

(println (x 2))
;; (println (x 20))

(defn parallel-count
  "Count the number of valid (s, e) pairs using reducers."
  [start-keys end-keys from-start from-end main-path-len example?]
  (reducers/fold
   + ;; Combining function
   (reducers/filter
    (fn [[s e]]
      (let [dist (manhattan-distance s e)
            total (+ (from-start s) (from-end e) dist)]
        (and (<= dist 20)
             (>= (- main-path-len total) (if example? 76 100)))))
    (reducers/mapcat
     (fn [s]
       (map (fn [e] [s e]) end-keys))
     start-keys))))

(parallel-count start-keys end-keys from-start from-end main-path-len example?)

;; (def main-path (path-to-end start end (next-w-fn inside-walls)))

;; (def distances-in-main-path (zipmap  main-path (range (dec (count main-path)) -1 -1)))



;; (defn path-try-dijkstra
;;   []
;;   (let []
;;     (loop [path main-path
;;            savings []]
;;       (if (empty? path)
;;         savings
;;         (let [p (first path)
;;               walls (walls-at-point p)]
;;           (println p (count walls))
;;           (recur (rest path) savings))))))

;; (path-try-dijkstra)


;; (def cand-walls
;;   (set (mapcat walls-at-point main-path)))


;; (defn dsd [inside-walls]
;;   (dijkstra-shortest-distances-with-d-1 start end  (next-w-fn inside-walls)))

;; (def dtoend (count main-path))

;; ;; (defn- passable? [inside-walls w]
;; ;;   (>= (count (filter (fn [[dr dc]]
;; ;;                        (inside-walls [(+ (first w) dr) (+ (second w) dc)]))
;; ;;                      [[1 0] [0 1] [-1 0] [0 -1]]))
;; ;;       2))

;; (def savings (map (fn [w]
;;                     (println "REMOVING" w)
;;                     (let [new-walls (disj inside-walls w)
;;                           dend (dsd new-walls)]
;;                       (- dtoend dend)))
;;                   cand-walls))


;; ;; (println savings)

;; (defn md
;;   [save]
;;   (count (filter (fn [d] (>= d save)) savings)))

;; (println (md (if example? 64 100)))









;; (defn distance-to-end
;;   [start new-neigh old-neigh removed-wall already-visited]
;;   (loop [start start
;;          visited (set/union #{start} already-visited)
;;          t 0
;;          removed? false]
;;     (if (= start end)
;;       t
;;       (let [ns (if removed?
;;                  (old-neigh start)
;;                  (new-neigh start))
;;             nexts (remove visited ns)
;;             at-wall? (and (not removed?)
;;                           ((set nexts) removed-wall))
;;             nexts (if at-wall?
;;                     [removed-wall]
;;                     nexts)
;;             removed? (if at-wall? true removed?)]
;;         (if (zero? (count nexts))
;;           nil
;;           (if (= 2 (count nexts))
;;             (let [dists (remove nil? (mapv (fn [n]
;;                                              (distance-to-end n old-neigh old-neigh removed-wall visited))
;;                                            nexts))]
;;               (+ t (apply min dists)))
;;             (let [n (first nexts)]
;;               (recur n (conj visited n) (inc t) removed?))))))))

;; ;; (def dsd (dijkstra-shortest-distances start (neighbors-dist inside-walls)))
;; (def dsd-to-end (distance-to-end start (next-w-fn inside-walls)
;;                                  (next-w-fn inside-walls)
;;                                  nil
;;                                  #{}))
;; ;; (def dtoend (dsd end))


;; ;; (def savings (map (fn [w]
;; ;;                     (let [new-walls (disj inside-walls w)
;; ;;                           d (dijkstra-shortest-distances start (neighbors-dist new-walls))
;; ;;                           dend (d end)]
;; ;;                       (- dtoend dend)))
;; ;;                   inside-walls))

;; (def new-savings (remove nil? (map (fn [removed-wall]
;;                                     ;;  (println "REMOVING" removed-wall)
;;                                      (let [new-walls (disj inside-walls removed-wall)
;;                                            dend (distance-to-end start
;;                                                                  (next-w-fn new-walls)
;;                                                                  (next-w-fn inside-walls)
;;                                                                  removed-wall
;;                                                                  #{})]
;;                                        (println "DEND" dend)
;;                                       ;;  CAN THE DIST BE NIL? OR is it a BUG?
;;                                        (if (some? dend) (- dsd-to-end dend) nil)))
;;                                    inside-walls)))

;; (when example?
;;   (let [f (frequencies (remove zero? new-savings))]
;;     (do
;;       (is (= f
;;              (frequencies (remove zero? '(38 64 40 0 0 0 36 2 0 4 0 2 0 8 2 10 0 0 0 0 0 0 0 0 2 12 0 0 0 0 0 6 0 2 0 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 4 0 4 0 0 0 4 6 4 0 0 2 4 0 12 2 0 0 2 0 0 0 0 4 0 0 4 4 0 0 0 2 4 0 2 0 2 4 0 20 0 0 0 0 0 0 0 0 0 0 0 8 0 0 0 0 8 0 0 0 0 2 4 0 0 0 0 2 0 0 0 12 0 2 4 0 0 0 8 0 0 4 0 0 0 0)))
;;                    ;
;;              ))
;;       (is (= 1 (f 64)))
;;       (is (= 1 (f 40)))
;;       (is (= 1 (f 38)))
;;       (is (= 1 (f 36)))
;;       (is (= 1 (f 20)))
;;       (is (= 3 (f 12)))
;;       ;
;;       )))

;; ;; ONLY NUMBERS
;; (def answer-1 nil)
;; (def answer-2 nil)
;; (defn- -main [& _]
;;   (println "Day XX, Part 1:" answer-1)
;;   (println "Day XX, Part 2:" answer-2))

;; ;; (-main)
