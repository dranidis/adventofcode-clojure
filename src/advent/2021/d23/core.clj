(ns advent.2021.d23.core
  (:require
   [advent.dijkstra :refer [dijkstra-shortest-distances]]
   [advent.util :refer [coords-of-symbol draw-grid grid-2d in-vector? set-grid
                        str->2D-no-trim]]
   [clojure.string :as str]
   [clojure.term.colors :as colors]))

(def example? false)
(def part2? true)

(def example "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(def input (if example? example (slurp "src/advent/2021/d23/input.txt")))

(def extra-lines "  #D#C#B#A#
  #D#B#A#C#")

(defn add-lines
  [input extra-lines]
  (let [lines (str/split-lines input)
        elines (str/split-lines extra-lines)]
    (str/join "\n" (concat (take 3 lines) elines (drop 3 lines)))))

;; GRID
(def grid (str->2D-no-trim (if part2? (add-lines input extra-lines) input)))
(def cols (count (first grid)))

(def hallway-row 1)

(def start {:A (vec (sort (coords-of-symbol grid "A")))
            :B (vec (sort (coords-of-symbol grid "B")))
            :C (vec (sort (coords-of-symbol grid "C")))
            :D (vec (sort (coords-of-symbol grid "D")))})

(def end-string "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########")

(def end-string-2 "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########")

(def end-grid (str->2D-no-trim (if part2? end-string-2 end-string)))
(def end {:A (vec (sort (coords-of-symbol end-grid "A")))
          :B (vec (sort (coords-of-symbol end-grid "B")))
          :C (vec (sort (coords-of-symbol end-grid "C")))
          :D (vec (sort (coords-of-symbol end-grid "D")))})


(def rows (count grid))
(defn draw-state
  [start]
  (-> (grid-2d rows cols " ")
      (set-grid (set (coords-of-symbol grid "#")) \#)
      (set-grid (get start :A) (colors/red \A))
      (set-grid (get start :B) (colors/blue \B))
      (set-grid (get start :C) (colors/yellow \C))
      (set-grid (get start :D) (colors/green \D))
      (draw-grid)))

(defn occupied
  [start]
  (set (mapcat (fn [k] (get start k)) [:A :B :C :D])))

(def pods (for [k [:A :B :C :D]
                n (if part2? [0 1 2 3] [0 1])]
            [k n]))

(defn assert-not-nil-all-pods
  [state]
  (let [c (every? (fn [pod] (not= (get-in state pod) nil)) pods)]
    (assert c (str "All pods must be in the state." state))))

(def destination-cols [3 5 7 9])

(defn destination-column
  [pod]
  (case (first pod)
    :A 3
    :B 5
    :C 7
    :D 9
    :error-destination))

(defn pods-in-rooms
  "Returns [keyw index column] for the pods in the rooms."
  [state]
  (for [pod pods
        :let [[k i] pod
              [r c] (get-in state pod)]
        :when (not= r hallway-row)]
    [k i c]))

(defn- pod-cost
  [pod]
  (case (first pod)
    :A 1
    :B 10
    :C 100
    :D 1000
    :error-cost))

(defn pod-in-hallway?
  [state pod]
  (= hallway-row (first (get-in state pod))))

(defn other-pods-in-des-c?
  [end pod]
  (seq
   (filter (fn [[kw _ c]]
             (and (= c (destination-column pod)) (not= kw (first pod))))
           (pods-in-rooms end))))

(defn free-route?
  [start hallway-route]
  (every? (fn [pos] (not ((occupied start) pos))) hallway-route))

(defn- route-from-hallway-to-room
  [start pod]
  (let [[r c] (get-in start pod)
        dest-c (destination-column pod)
        room-r (inc hallway-row)
        range-c (if (< c dest-c)
                  (range (inc c) (inc dest-c))
                  (range (dec c) (dec dest-c) -1))
        hallway-route (vec (for [c range-c]
                             [hallway-row c]))]
    (if (and (seq hallway-route)
             (free-route? start hallway-route))
      (let [rrs (for [rr (range room-r (+ 4 room-r))
                      :when (not ((occupied start) [rr dest-c]))]
                  rr)
            column-end (apply max rrs)
            column-route (vec (for [rr (range room-r (inc column-end))]
                                [rr dest-c]))]
        (apply conj hallway-route column-route))
      [])))

(defn- routes-from-room-to-hallway
  [state pod]
  (let [[r c] (get-in state pod)]
    (if ((occupied state) [(dec r) c]) ;; blocked by other pod
      []
      (let [left-hallway-ends
            (for [hc (range (dec c) 0 -1)
                  :when (not ((occupied state) [hallway-row hc]))
                  :when (not (in-vector? destination-cols hc))]
              hc)

            left-hallway-routes
            (filter
             (partial free-route? state)
             (for [end left-hallway-ends]
               (vec (for [rc (range c (dec end) -1)]
                      [hallway-row rc]))))

            right-hallway-ends
            (for [hc (range (inc c) (dec cols))
                  :when (not ((occupied state) [hallway-row hc]))
                  :when (not (in-vector? destination-cols hc))]
              hc)

            right-hallway-routes
            (filter
             (partial free-route? state)
             (for [end right-hallway-ends]
               (vec (for [rc (range c (inc end) 1)]
                      [hallway-row rc]))))

            routes (if (seq left-hallway-routes)
                     left-hallway-routes
                     [])
            routes (if (seq right-hallway-routes)
                     (apply conj routes right-hallway-routes)
                     routes)]
        ;; (println "Pod" pod [r c] "Routes:" left-hallway-routes)

        (if (seq routes)
          (let [column-route (vec (for [rr (range (dec r) hallway-row -1)]
                                    [rr c]))]
            (for [route routes
                  :when (seq route)]
              (apply conj column-route route)))
          [])))))

(defn- settled? [end pod]
  (let [[r c] (get-in end pod)]
    (and (= c (destination-column pod))
         (or (= r (inc (inc hallway-row)))
             (and (= r (inc hallway-row))
                  (not (other-pods-in-des-c? end pod)))))))

(defn- next-positions
  "Returns a list of [position cost] pairs for
   the next positions of a pod."
  [end pod]
  (if (pod-in-hallway? end pod)
    (if (other-pods-in-des-c? end pod)
      []
      (let [route (route-from-hallway-to-room end pod)]
        ;; (println "Route:" route)
        (if (last route)
          [[(last route) (* (count route) (pod-cost pod))]]
          [])))
    (if (settled? end pod)
      []
      (let [routes (routes-from-room-to-hallway end pod)]
        (if routes
          (for [route routes
                :when (seq route)]
            [(last route) (* (count route) (pod-cost pod))])
          [])))))

(defn- sort-pos
  "Sorts the positions of each keyw in the state."
  [state]
  (into {} (for [[k v] state]
             [k (vec (sort v))])))

(defn neighbors
  [start]
  (assert-not-nil-all-pods start)
  (map (fn [[c s]] [c (sort-pos s)])
       (reduce (fn [acc pod]
                 (reduce (fn [acc2 [pos cost]]
                           (assert (some? pos)
                                   (str "pos must not be nil."
                                        start pod acc acc2 "P" pos
                                        "NP" (next-positions start pod)))
                           (conj acc2
                                 [cost (assoc-in start pod pos)]))
                         acc
                         (next-positions start pod)))
               []
               pods)))

(comment
  (neighbors start)
  (neighbors end)
  (clojure.pprint/pp)

  (def pod [:D 0])
  (next-positions start pod)
  (next-positions end pod)
  (routes-from-room-to-hallway start pod)
  (get-in start pod)
  ((occupied start) [1 3])
  ;
  )


(def dsd (dijkstra-shortest-distances start neighbors))
(println (count dsd))

(println (dsd end))


(def answer-1 nil)
(def answer-2 nil)
(defn- -main [& _]
  (println "Day XX, Part 1:" answer-1)
  (println "Day XX, Part 2:" answer-2))

(-main)


(loop [start start
       steps 0
       cost 0
       totalcost 0]
  (println "Cost:" cost "Total Cost:" totalcost)
  (draw-state start)
  (if (= start end)
    steps
    (let [nb (neighbors start)]
      (if (empty? nb)
        steps
        (let  [i (rand-int (count nb))
               [c n] (nth nb i)]
          (recur n (inc steps) c (+ totalcost c)))))))