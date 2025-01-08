(ns advent.dijkstra
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

(defn dijkstra-shortest-distance-to-end-pred
  "Return the shortest distances from start to all other nodes.
   
   - start: node
   - end-pred: pred for the target node
   - neighbors: node -> [[distance neighbor]]
   - returns: distance to end

   The neighbors is a function mapping a node to its 
   neighbors, which are a list of [distance neighbor] pairs.
   The result is a map from destination node to distance from the start."
  [start end-pred neighbors]
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
          (if (end-pred node)
            dist
            (if (get D node)
              (recur Q D)
              (let [D (assoc D node dist)
                    Q (apply add-with-priority! Q
                             (map (fn [[d n]]
                                  ;; (println "Adding" n "with distance" (+ dist d))
                                    [(+ dist d) n])
                                  (neighbors node)))]
                (recur Q D)))))
        D))))

(defn A-star-shortest-distance-to-end-pred
  "Return the shortest distance from start to end note.
   
   - start: node
   - end-pred: pred for the target node
   - neighbors: node -> [[distance neighbor]]
   - heuristics: function for A* (fn [x] 0) defaults to dijkstra
   - returns: distance to end

   The neighbors is a function mapping a node to its 
   neighbors, which are a list of [distance neighbor] pairs.
   The result is a map from destination node to distance from the start."
  [start end-pred neighbors heuristics]
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2)))
                              [(heuristics start) 0 start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[_ dist node] (extract-min! Q)]
          (if (end-pred node)
            dist
            (if (get D node)
              (recur Q D)
              (let [D (assoc D node dist)
                    Q (apply add-with-priority! Q
                             (map (fn [[d n]]
                                    [(+ dist d (heuristics n)) (+ dist d) n])
                                  (neighbors node)))]
                (recur Q D)))))
        D))))

(defn dijkstra-shortest-distances-pred
  "Returns a map from destination node to [distance previous-node].
   
   - start: node 
   - neighbors: node -> [[distance neighbor]]
   - returns: {destination-node [distance previous-node]}

   The distance is the shortest distance from start to the destination node.
   The neighbors if a function mapping a node to its 
   neighbors, which are a list of [distance neighbor] pairs."
  [start neighbors]
  ;; Q is a priority queue of [distance previous-node node]
  ;; D is a map from node to [distance previous-node]
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2))) [0 nil start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist prev node] (extract-min! Q)]
      ;;     (println "Current node" node "distance" dist "queue" Q "prev" prev)
      ;;     (println "Distances" D)

          ;; If we have already visited the node, skip it.
          (if (get D node)
            (recur Q D)
            (let [D (assoc D node [dist prev])
                  Q (apply add-with-priority! Q
                           (map (fn [[d n]] [(+ dist d) node n])
                                (neighbors node)))]
              (recur Q D))))
        D))))

(comment

  (defn neighbors
    "Return the neighbors of a node.
   The neighbors are a list of [distance neighbor] pairs."
    [n]
    (case n
      "A" [[1 "B"] [3 "C"] [4 "E"]]
      "B" [[1 "A"] [4 "D"] [2 "E"]]
      "C" [[3 "A"] [2 "F"]]
      "D" [[4 "B"] [2 "F"]]
      "E" [[4 "A"] [1 "F"] [2 "B"]]
      "F" [[1 "C"] [1 "E"] [2 "D"]]))


  (neighbors "A")
  (dijkstra-shortest-distances "A" neighbors)
  ;; {"A" 0, "B" 1, "C" 3, "E" 3, "F" 4, "D" 5}

  (dijkstra-shortest-distance-to-end-pred "A" (fn [n] (= n "D")) neighbors)

  (defn neighbors-m
    "Return the neighbors of a node.
       The neighbors are a list of [distance neighbor] pairs."
    [n]
    (case n
      {:A 0} [[1 {:B 0}] [3 {:C 0}] [4 {:E 0}]]
      {:B 0} [[1 {:A 0}] [4 {:D 0}] [2 {:E 0}]]
      {:C 0} [[3 {:A 0}] [2 {:F 0}]]
      {:D 0} [[4 {:B 0}] [2 {:F 0}]]
      {:E 0} [[4 {:A 0}] [1 {:F 0}] [2 {:B 0}]]
      {:F 0} [[1 {:C 0}] [1 {:E 0}] [2 {:D 0}]]))

  (dijkstra-shortest-distances-pred {:A 0} neighbors-m)
  ;; {"A" [0 nil], "B" [1 "A"], "C" [3 "A"], "E" [3 "B"], "F" [4 "E"], "D" [5 "B"]}

  ;
  )

(defn shortest-path-from-to
  "Return the shortest path from start to end.
   The path is a vector of nodes.
   The neighbors if a function mapping a node to its 
   neighbors, which are a list of [distance neighbor] pairs."
  [neighbors start to]
  (let [D (dijkstra-shortest-distances-pred start neighbors)]
    (vec (loop [to to
                path (list to)]
           (if (= to start)
             path
             (let [to (second (get D to))]
               (recur to (conj path to))))))))

(comment
  (shortest-path-from-to neighbors "A" "F")
  ;; ["A" "B" "E" "F"]

  ;
  )

(defn dijkstra-shortest-distances-pred-set
  "Returns a map from destination node to [distance {previous-nodes}].
   
   - start: node 
   - neighbors: node -> [[distance neighbor]]
   - returns: map{destination-node [distance set{previous-nodes}]}
   
   "
  [start neighbors]
  ;; Q is a priority queue of [distance previous-node current-node]
  ;; D is a map from node to [distance predecessors]
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2))) [0 nil start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist prev node] (extract-min! Q)
             ;; Check if we've already visited the node with this distance
              current-entry (get D node)
              current-dist (first current-entry)]
          (if current-entry
            (if (= dist current-dist)
            ;; If the node has already been visited with the same distance, 
              ;; add `prev` to predecessors.
              (recur Q (update D node (fn [[d preds]] [d (conj preds prev)])))
              ;; else skip it
              (recur Q D))

            ;; If the node is visited with a shorter distance, update distance and predecessors.
            (let [D (assoc D node [dist (if prev #{prev} #{})])
                  Q (apply add-with-priority! Q
                           (map (fn [[d n]] [(+ dist d) node n])
                                (neighbors node)))]
              (recur Q D))))
        D))))


(defn all-shortest-paths-from-to
  "Return ALL shortest paths from start to end as a list of vectors.
   The neighbors is a function mapping a node to its neighbors,
   which are a list of [distance neighbor] pairs."
  [neighbors start to]
  (let [D (dijkstra-shortest-distances-pred-set start neighbors)]
    (letfn [(backtrack [node]
              (if (= node start)
                [[start]]
                (for [pred (second (get D node))  ;; Get all predecessors
                      path (backtrack pred)]    ;; Recursively backtrack returns a list of paths
                  (conj path node))))]
      (backtrack to))))


(comment
  (defn example-neighbors [node]
    (case node
      :A [[1 :B] [4 :C]]
      :B [[2 :C] [5 :D]]
      :C [[2 :D]]
      :D []))

  (defn exc-n [node]
    (case node
      :A [[1 :B] [1 :C]]
      :B [[1 :D]]
      :C [[1 :D]]
      :D []))

  (dijkstra-shortest-distances-pred :A exc-n)
  ;; {:A [0 nil], :B [1 :A], :C [1 :A], :D [2 :B]}

  (dijkstra-shortest-distances-pred-set :A exc-n)
  ;; {:A [0 #{}], :B [1 #{:A}], :C [1 #{:A}], :D [2 #{:B :C}]}

  (shortest-path-from-to exc-n :A :D)
  ;; [:A :B :D]

  (all-shortest-paths-from-to exc-n :A :D)
  ;; ([:A :B :D] [:A :C :D])

  ;
  )