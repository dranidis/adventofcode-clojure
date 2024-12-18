(ns advent.2021.d23.core
  (:require
   [advent.dijkstra :refer [dijkstra-shortest-distances]]
   [advent.util :refer [coords-of-symbol str->2D-no-trim]]))

(def example? false)

(def example "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(def input (if example? example (slurp "src/advent/2021/d23/input.txt")))

;; GRID
(def grid (str->2D-no-trim input))
(def rows (count grid))
(def cols (count (first grid)))

(def Ws (set (coords-of-symbol grid "#")))
;; (def Ds (coords-of-symbol grid "."))

(def startA (coords-of-symbol grid "A"))
(def startB (coords-of-symbol grid "B"))
(def startC (coords-of-symbol grid "C"))
(def startD (coords-of-symbol grid "D"))
(def start {:A startA
            :B startB
            :C startC
            :D startD})

(def end-string "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########")
(def end-grid (str->2D-no-trim end-string))
(def endA (coords-of-symbol end-grid "A"))
(def endB (coords-of-symbol end-grid "B"))
(def endC (coords-of-symbol end-grid "C"))
(def endD (coords-of-symbol end-grid "D"))
(def end {:A endA
          :B endB
          :C endC
          :D endD})

(defn occupied
  [state]
  (set (mapcat (fn [k] (get state k)) [:A :B :C :D])))

(comment
  (def keyw :A)
  ;
  )

(def drcs [[0 1] [1 0] [0 -1] [-1 0]])
(def pods (for [k [:A :B :C :D]
                n [0 1]]
            [k n]))

(defn destination-column
  [pod]
  (case (first pod)
    :A 3
    :B 5
    :C 7
    :D 9
    :error-destination))

(defn in-room
  [state keyw]
  (let [c (destination-column key)]
    (map (fn [r] ()) [2 3])))

(defn- cost-fn [pod]
  (case (first pod)
    :A 1
    :B 10
    :C 100
    :D 1000
    :error-cost))

(defn immediately-next
  [state pod]
  (let [[r c] (get-in state pod)
        cost (cost-fn pod)]
    ;; (println r c)
    (vec (for [[dr dc] drcs
               :let [nr (+ r dr)
                     nc (+ c dc)]
               :when (not ((occupied state) [nr nc]))
               :when (not (Ws [nr nc]))]
           [cost [nr nc]]))))

(immediately-next start [:D 0])



(defn- next-positions
  "Returns a list of [position cost] pairs for
   the next positions of a pod."
  [state pod]
  (let [in-hallway? (= 1 (first (get-in state pod)))
        [cost n-pos] (immediately-next state pod)]
    (loop [pos (get-in state pod)
           next-positions-queue n-pos
           positions [pos]]
      (if (empty? next-positions-queue)
        positions
        (let [])))))

(defn neignbors
  [state]
  (for [pod pods
        [pod-position cost] (next-positions state pod)]
    [cost (assoc state pod pod-position)]))





(moves start :A)
(moves start :B)
(moves start :C)
(moves start :D)

(defn neignbors
  [state]
  (for [mA (moves state :A)]
    (assoc state :A mA)))

;; (dijkstra-shortest-distances start neignbors)


(def answer-1 nil)
(def answer-2 nil)
(defn- -main [& _]
  (println "Day XX, Part 1:" answer-1)
  (println "Day XX, Part 2:" answer-2))

(-main)
