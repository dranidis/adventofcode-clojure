(ns advent.2023.d17.dijkstra
  (:require [advent.2023.d17.priority-queue :refer [add-with-priority!
                                                    extract-min! make-priority-queue!]]))

;; (def ^:private inf (Long/MAX_VALUE))

;; (defn dijkstra [g src]
;;   (let [dist (assoc (zipmap (keys g) (repeat inf)) src 0)
;;         Q (apply conj (PersistentQueue/EMPTY) (keys g))]
;;     (loop [Q Q
;;            dist dist]
;;       (println (count Q))
;;       (if (empty? Q)
;;         dist
;;         (let [Qsorted (sort-by (fn [v] (get dist v)) Q)
;;               u (first Qsorted)
;;               Q (rest Qsorted)
;;               dist (merge dist
;;                           (into {}
;;                                 (for [v Q
;;                                       :when (get-in g [u v])]
;;                                   [v (let [alt (+ (get dist u) (get-in g [u v]))
;;                                            dist-v (get dist v)]
;;                                        (if (< alt dist-v) alt dist-v))])))]
;;           (recur Q dist))))))

(defn dijkstra-shortest-distances [start neighbors]
  (let [Q (add-with-priority! (make-priority-queue!) [0 start])
        D {}]
    (loop [Q Q
           D D]
      (if (not (empty? Q))
        (let [[dist node] (extract-min! Q)]
      ;;     (println "Current node" node "distance" dist "queue" Q)
      ;;     (println "Distances" D)
          (if (get D node)
            (recur Q D)
            (let [D (assoc D node dist)
                  Q (apply add-with-priority! Q
                           (map (fn [[d n]] [(+ dist d) n])
                                (neighbors node)))]
              (recur Q D))))
        D))))

(defn dijkstra-shortest-distances-path [start neighbors]
  (let [Q (add-with-priority! (make-priority-queue!) [0 nil start])
        D {}]
    (loop [Q Q
           D D]
      (if (not (empty? Q))
        (let [[dist prev node] (extract-min! Q)]
      ;;     (println "Current node" node "distance" dist "queue" Q "prev" prev)
      ;;     (println "Distances" D)
          (if (get D node)
            (recur Q D)
            (let [D (assoc D node [dist prev])
                  Q (apply add-with-priority! Q
                           (map (fn [[d n]] [(+ dist d) node n])
                                (neighbors node)))]
              (recur Q D))))
        D))))

(defn neighbors [n]
  (case n
    "A" [[1 "B"] [3 "C"] [4 "E"]]
    "B" [[1 "A"] [4 "D"] [2 "E"]]
    "C" [[3 "A"] [2 "F"]]
    "D" [[4 "B"] [2 "F"]]
    "E" [[4 "A"] [1 "F"] [2 "B"]]
    "F" [[1 "C"] [1 "E"] [2 "D"]]))

(dijkstra-shortest-distances "A" neighbors)

(defn shortest-path-from-to [start to]
  (let [D (dijkstra-shortest-distances-path start neighbors)]
    (loop [to to
           path (list to)]
      (if (= to start)
        path
        (let [to (second (get D to))]
          (recur to (conj path to)))))))

(shortest-path-from-to "A" "F")