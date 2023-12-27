(ns advent.2023.d17.core
  (:require [advent.2023.d17.priority-queue :refer [add-with-priority!
                                                    change-value! extract-min!
                                                    make-priority-queue! queue->list]]
            [clojure.string :as str])
  (:import [clojure.lang PersistentQueue]))

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

;; (def input "24
;; 13")

;; (def input "243
;; 131
;; 514")


(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
                (parse-long (str c)))))))

(def the-map (str->2D input))

(def num-cols (count (first the-map)))
(def num-rows (count the-map))

(def start {:r 0 :c 0 :total-dir 0 :dir :rt :heat-loss 0
            :goal-r (dec num-rows) :goal-c (dec num-cols)
            :pr nil :pc nil
            :visited-coords #{}})

(defn upd-dir [state new-dir]
  (if (= new-dir (:dir state))
    (update state :total-dir inc)
    (assoc state :total-dir 1 :dir new-dir)))

(defn move [state mv]
  (let [new-state
        (case mv
          :rt (-> state (update :c inc) (upd-dir mv))
          :lf (-> state (update :c dec) (upd-dir mv))
          :up (-> state (update :r dec) (upd-dir mv))
          :dn (-> state (update :r inc) (upd-dir mv)))
        new-r (:r new-state)
        new-c (:c new-state)]
    (-> new-state
        (update :heat-loss + (get-in the-map [new-r new-c]))
        (assoc :pr (:r state) :pc (:c state))
        (update :visited-coords conj [(:r state) (:c state)]))))

(defn is-move-valid? [state mv]
  (letfn [(less-than-3? [d]
            (or (not= d (:dir state)) (< (:total-dir state) 3)))
          (not-in-path? [d]
            (let [new-state (move state d)]
              (not ((:visited-coords state) [(:r new-state) (:c new-state)]))))]
    (case mv
      :rt (and (less-than-3? mv) (< (:c state) (dec num-cols)) (not-in-path? mv))
      :lf (and (less-than-3? mv) (> (:c state) 0) (not-in-path? mv))
      :up (and (less-than-3? mv) (> (:r state) 0) (not-in-path? mv))
      :dn (and (less-than-3? mv) (< (:r state) (dec num-rows)) (not-in-path? mv)))))

(defn possible-moves [state]
  (set (filter (partial is-move-valid? state) [:rt :lf :up :dn])))

(defn dfs [state visited-coords]
  ;; (prn state)1
  (let [heat-loss (if (= state start) 0
                      (get-in the-map [(:r state) (:c state)]))]
    (if (and (= (:r state) (:goal-r state))
             (= (:c state) (:goal-c state)))
      heat-loss
      (let [moves (possible-moves state)
            next-states (map (partial move state) moves)
            not-visited-states
            (remove (fn [s]
                      (visited-coords [(:r s) (:c s)]))
                    next-states)]
        (+ heat-loss
           (let [all (map #(dfs % (conj visited-coords [(:r %) (:c %)]))
                          not-visited-states)]
             (if (empty? all) 0
                 (apply min all))))))))

(defn dfs-loop [state]
  (loop [t 0
         state-stack [state]
        ;;  visited-coords #{}
         min-heat-loss inf]
    (if (empty? state-stack)
      min-heat-loss
      (let [current-state (peek state-stack)
            moves (possible-moves current-state)
            next-states (map (partial move current-state) moves)
            not-visited-states
            (remove (fn [s]
                      ;; (visited-coords [(:r s) (:c s)])
                      false)
                    next-states)]

        (println (count state-stack) min-heat-loss t)
        (println current-state)
        (cond (and (= (:r current-state) (:goal-r current-state))
                   (= (:c current-state) (:goal-c current-state)))
              (let [heat-loss (:heat-loss current-state)]
                (println "REACHED END")
                (if (< heat-loss min-heat-loss)
                  (recur (inc t) (pop state-stack)
                    ;;  visited-coords
                         heat-loss)
                  (recur (inc t) (pop state-stack)
                    ;;  visited-coords
                         min-heat-loss)))
              (>= (inc (:heat-loss current-state)) min-heat-loss)
              (do
                (println "CUT")
                (recur (inc t) (pop state-stack) min-heat-loss))
              :else (recur (inc t) (apply conj (pop state-stack) not-visited-states)
                ;;  (conj visited-coords [(:r current-state) (:c current-state)])
                           min-heat-loss))))))

;; (dfs-loop start)

(comment
  (dfs start #{})
  1
  ;
  )

;; (defn dijkstra []
;;   )
;; (pop (conj (conj (PersistentQueue/EMPTY) 1 2 3) 4))

;; the-map

(def all-nodes (for [r (range num-rows)
                     c (range num-cols)
                     dir [:rt :lf :up :dn]
                     tot (range 3)]
                 [r c dir tot]))

;; (count all-nodes)

;; (count (for [source all-nodes
;;       dest  all-nodes]
;;   [source dest]))




;; (defn dijkstra [g src]
;;   (loop [dsts (assoc (zipmap (keys g) (repeat nil)) src 0)
;;          curr src
;;          unvisited (apply hash-set (keys g))]
;;     (println "dsts" dsts) (println "curr" curr) (println "unvi" unvisited)
;;     (if (empty? unvisited)
;;       dsts
;;       (let [unvisited-without-current  (disj unvisited curr)
;;             _ (prn "U" (sort-by #(get dsts %) unvisited-without-current))
;;             first-unvisited (first (sort-by #(get dsts %) unvisited-without-current))
;;             nrds (zipmap (keys g) (map #(select-keys % unvisited-without-current) (vals g)))]
;;         (pprint/pprint (get nrds curr))
;;         (pprint/pprint first-unvisited)
;;         (if (empty? (get nrds curr))
;;           (recur dsts first-unvisited unvisited-without-current)
;;           (let [cdst (get dsts curr)
;;                 _ (println "cdst" cdst)
;;                 roads (select-keys (get g curr) unvisited-without-current)
;;                 _ (println "roads" roads)
;;                 reslt (zipmap (keys dsts)
;;                               (map #(if-let [rd (get roads %)]
;;                                       (let [_ (println "rd" rd)
;;                                             idst (get dsts %)
;;                                             sum  (+ cdst rd)]
;;                                         (if (or (nil? idst)
;;                                                 (< sum idst))
;;                                           sum idst))
;;                                       (get dsts %)) (keys dsts)))]
;;             (recur reslt first-unvisited unvisited-without-current)))))))

(comment

  (def src [0 0])
  (def dist (assoc (zipmap (keys g) (repeat inf)) src 0))
  (def Q (apply conj (PersistentQueue/EMPTY) (keys g)))
  (def Qsorted (sort-by (fn [v] (get dist v)) Q))
  (def u (first Qsorted))
  (def Q (rest Qsorted))
  (get-in g [u [2 2]])
;
  )

(println "Creating graph")

(def g (into {} (for [r (range num-rows)
                      c (range num-cols)]
                  {[r c]
                   (into {} (for [dir [[0 1] [1 0] [0 -1] [-1 0]]
                                  :let [r' (+ r (first dir))
                                        c' (+ c (second dir))]
                                  :when (and (<= 0 r' (dec num-rows))
                                             (<= 0 c' (dec num-cols)))]
                              {[r' c'] (get-in the-map [r' c'])}))})))

(defn dijkstra [g src]
  (let [dist (assoc (zipmap (keys g) (repeat inf)) src 0)
        Q (apply conj (PersistentQueue/EMPTY) (keys g))]
    (loop [Q Q
           dist dist]
      (println (count Q))
      (if (empty? Q)
        dist
        (let [Qsorted (sort-by (fn [v] (get dist v)) Q)
              u (first Qsorted)
              Q (rest Qsorted)
              dist (merge dist
                          (into {}
                                (for [v Q
                                      :when (get-in g [u v])]
                                  [v (let [alt (+ (get dist u) (get-in g [u v]))
                                           dist-v (get dist v)]
                                       (if (< alt dist-v) alt dist-v))])))]
          (recur Q dist))))))

(defn dijkstra-prio [g src]
  (let [dist (assoc (zipmap (keys g) (repeat inf)) src 0)
        Q (make-priority-queue!)
        _ (mapv (fn [v] (add-with-priority! Q v (get dist v))) (keys g))]
    (loop [Q Q
           dist dist]
      (if (empty? Q)
        dist
        (let [u (extract-min! Q)
              dist (merge dist
                          (into {}
                                (for [v (queue->list Q)
                                      :when (get-in g [u v])]
                                  [v
                                   (let [alt (+ (get dist u) (get-in g [u v]))
                                         dist-v (get dist v)]
                                     (if (< alt dist-v)
                                       (do
                                         (change-value! Q v dist-v alt)
                                         alt)
                                       dist-v))])))]
          (recur Q dist))))))



(println "Running diskjstra")

(println "RESULT" (get (dijkstra-prio g [0 0]) [(dec num-rows) (dec num-cols)]))

;; (def dsts {[2 2] nil, [0 0] 0, [1 0] nil, [1 1] nil, [0 2] nil, [2 0] nil, [2 1] nil, [1 2] nil, [0 1] nil})
;; (get dsts [2 2])

(/ 307 60.0)