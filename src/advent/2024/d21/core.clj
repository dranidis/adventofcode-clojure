(ns advent.2024.d21.core
  (:require
   [advent.2023.d17.priority-queue :refer [add-with-priority! extract-min!
                                           make-priority-queue!]]
   [advent.util :refer [coords-of-symbol str->2D str->nums]]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(def example? false)
(def example "029A
980A
179A
456A
379A")

(def input (if example? example (slurp "src/advent/2024/d21/input.txt")))
(def codes (str/split-lines input))

;; DIJKSTRA
;; (defn path-from-to
;;   [neighbors start end]
;;   ;; Q is a priority queue of [distance previous-node node]
;;   ;; D is a map from node to [distance previous-node]
;;   (let [Q (add-with-priority! (make-priority-queue!
;;                                (fn [[d _] [d2 _]] (< d d2))) [0 nil start ""])
;;         D {}]
;;     (loop [Q Q
;;            D D]
;;       (if-not (empty? Q)
;;         (let [[dist prev node cmdstr] (extract-min! Q)]
;;           ;; (println "Current node" node "distance" dist "queue" Q "prev" prev)
;;           ;; (println "Distances" D)

;;           ;; If we have already visited the node, skip it.
;;           (if (= node end)
;;             cmdstr;;             ;; (shortest-path-from-to (assoc D node [dist prev]) start end)
;;             (if (get D node);;               (recur Q D)
;;               (let [D (assoc D node [dist prev])
;;                     Q (apply add-with-priority! Q
;;                              (map (fn [[n cmd]] [(inc dist) node n (str cmdstr cmd)])
;;                                   (neighbors node)))]
;;                 (recur Q D)))))
;;         D))))

;; (defn all-paths-from-to
;;   [neighbors start end]
;;   ;; Q is a priority queue of [distance previous-node node]
;;   ;; D is a map from node to [distance previous-node]
;;   (let [Q (add-with-priority! (make-priority-queue!
;;                                (fn [[d _] [d2 _]] (< d d2))) [0 nil start ""])
;;         D {}]
;;     (loop [Q Q
;;            D D]
;;       (if-not (empty? Q)
;;         (let [[dist prev node cmdstr] (extract-min! Q)
;;               ns (neighbors node)
;;               cmds (map (fn [[n cmd]] (str cmdstr cmd)) ns)]
;;           (if (get D node)
;;             (let [pdist (first (get D node))]
;;               (if (= dist pdist)
;;                 (recur Q (update D node
;;                                  (fn [[d preds cs]]
;;                                    [d (conj preds prev)
;;                                     (conj cs cmdstr)])))

;;                 (recur Q D)))
;;             (let [D (assoc D node [dist (if prev #{prev} #{}) cmds])
;;                   D (assoc D node [dist prev])
;;                   Q (apply add-with-priority! Q
;;                            (map (fn [[n cmd]] [(inc dist) node n (str cmdstr cmd)])
;;                                 (neighbors node)))]
;;               (recur Q D))))
;;         (get D end)))))

(defn dijkstra-shortest-distances-pred-set
  "Returns a map from destination node to [distance {previous-nodes}].

   - start: node
   - neighbors: node -> [[distance neighbor]]
   - returns: map{destination-node [distance set{previous-nodes}]}

   "
  [start neighbors]
  ;; Q is a priority queue of [distance previous-node current-node]
  ;; D is a map from node to [distance predecessors and commands]
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2))) [0 nil start])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist prev node] (extract-min! Q)
              [n cmd] node
             ;; Check if we've already visited the node with this distance
              current-entry (get D n)
              current-dist (first current-entry)]
          (if current-entry
            (if (= dist current-dist)
            ;; If the node has already been visited with the same distance,
              ;; add `prev` to predecessors.
              (recur Q (update D n (fn [[d preds]]
                                     [d (conj preds [prev cmd])])))
              ;; else skip it
              (recur Q D))

            ;; If the node is visited with a shorter distance, update distance and predecessors.
            (let [D (assoc D n [dist [[prev cmd]]])
                  Q (apply add-with-priority! Q
                           (map (fn [[d neighbor]]
                                  ;; (println "At node " node "Adding" n "with distance" d)
                                  [(+ dist d) node neighbor])
                                (neighbors node)))]
              (recur Q D))))
        D))))

(defn all-shortest-paths-from-to
  "Return ALL shortest paths from start to end as a list of vectors.
   The neighbors is a function mapping a node to its neighbors,
   which are a list of [distance neighbor] pairs."
  [neighbors start to]
  (let [D (dijkstra-shortest-distances-pred-set start neighbors)]
    (letfn [(bt
              [node]
              (if (= node (first start))
                [""]
                (for [[[pred c] cmd] (second (get D node))
                      pre (bt pred)]
                  (str pre cmd))))]
      (bt to))))



;;;;;;;;;;;;;;;;;;
;; KEYPAD

(def num-keypad
  (str->2D
   "789
    456
    123
    #0A"))

;; (defn num-keypad-next
;;   [[r c]]
;;   (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
;;         :let [nr (+ r dr)
;;               nc (+ c dc)
;;               cell (get-in num-keypad [nr nc])]
;;         :when (and (some? cell)
;;                    (not= cell "#"))]
;;     [[nr nc] cmd]))

(defn num-keypad-next-dist
  [[[r c] cm]]
  (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
        :let [nr (+ r dr)
              nc (+ c dc)
              cell (get-in num-keypad [nr nc])]
        :when (and (some? cell)
                   (not= cell "#"))]
    [1 [[nr nc] cmd]]))

(defn num->pos
  [char]
  (first (coords-of-symbol num-keypad char)))

(def position-on-num-keypad
  (assoc (zipmap (map str (range 10))
                 (map num->pos (map str (range 10))))
         "A" [3 2]))

(def num-keypad-start [(position-on-num-keypad "A") ""])


(comment
  (def d0 ((dijkstra-shortest-distances-pred-set
            num-keypad-start
            num-keypad-next-dist) (num->pos "0")))

  (all-shortest-paths-from-to num-keypad-next-dist
                              num-keypad-start (num->pos "4"))
  ;
  ;
  )

;;
;;
;;

(def dir-keypad
  (str->2D
   "#^A
    <v>"))


;; (defn dir-keypad-next
;;   [[r c]]
;;   ;;  <
;;   (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
;;         :let [nr (+ r dr)
;;               nc (+ c dc)
;;               cell (get-in dir-keypad [nr nc])]
;;         :when (and (some? cell)
;;                    (not= cell "#"))]
;;     [[nr nc] cmd]))

(defn dir-keypad-next-dist
  [[[r c] cm]]
  (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
        :let [nr (+ r dr)
              nc (+ c dc)
              cell (get-in dir-keypad [nr nc])]
        :when (and (some? cell)
                   (not= cell "#"))]
    [1 [[nr nc] cmd]]))

(defn position-of-char-on-dir-keypad
  [char]
  (first (coords-of-symbol dir-keypad char)))

(def position-on-dir-keypad
  (assoc (zipmap ["^" "v" "<" ">"]
                 (map position-of-char-on-dir-keypad ["^" "v" "<" ">"]))
         "A" [0 2]))

;; (def dir-keypad-start (position-on-dir-keypad "A"))
(def dir-keypad-start [(position-on-dir-keypad "A") ""])

;; (defn get-seq-to-type-a-char-on-num-keypad
;;   "Receives a
;;    - character to type
;;    - and the starting position on the keypad
;;    and returns
;;    - a string of one of the shorterst sequences to perform on the directional keypad
;;    - the final position on the keypad.
;;    e.g. (type-on-numeric-keypd 0) returns <A"
;;   [to-type start]
;;   (str (path-from-to num-keypad-next start (num->pos to-type)) "A"))

(defn get-all-seq-to-type-a-char-on-num-keypad
  [to-type start]
  (map (fn [s] (str s "A"))
       (all-shortest-paths-from-to num-keypad-next-dist start (num->pos to-type))))


(comment
  (get-all-seq-to-type-a-char-on-num-keypad "8" num-keypad-start)
;
  )

;; (defn num->dir
;;   [str-to-type start]
;;   (loop [str-to-type str-to-type
;;          start start
;;          seq-cmds ""]
;;     (if (empty? str-to-type)
;;       seq-cmds
;;       (let [to-type (str (first str-to-type))
;;             seq-cmd (get-seq-to-type-a-char-on-num-keypad to-type start)]
;;         (recur (rest str-to-type)
;;                (position-on-num-keypad to-type)
;;                (str seq-cmds seq-cmd))))))

(defn num->dir
  [str-to-type start]
  (letfn [(num->dir-rec [str-to-type start seq-cmd]
            ;; (println "Str to type" str-to-type "Start" start "Seq-cmd" seq-cmd)
            (if (empty? str-to-type)
              seq-cmd
              (let [to-type (str (first str-to-type))
                    ;; _ (println "To type" to-type "Start" start)
                    seq-cmds (get-all-seq-to-type-a-char-on-num-keypad
                              to-type start)]
                ;; (println "To type" to-type "Seq-cmds" seq-cmds)
                (let [cls (flatten (mapv (fn [sq]
                                           (num->dir-rec (subs str-to-type 1)
                                                         [(position-on-num-keypad to-type) ""] sq))
                                         seq-cmds))]
                  ;; (prn "Cls" cls "Seq-cmd" seq-cmd)
                  ;; (prn "returning " (mapv (fn [s] (str seq-cmd s)) cls))
                  (mapv (fn [s] (str seq-cmd s)) cls)))))]
    (num->dir-rec str-to-type start "")))

(is (= #{"<A^A>^^AvvvA" "<A^A^>^AvvvA" "<A^A^^>AvvvA"}
       (set (num->dir "029A" num-keypad-start))))
(comment
  (empty? "")
  (def str-to-type "029A")

  (num->dir "A" num-keypad-start)
  (num->dir "0A" num-keypad-start)
  (num->dir "02A" num-keypad-start)
  (num->dir "029A" num-keypad-start)
  (all-shortest-paths-from-to num-keypad-next-dist [3 1] (num->pos "2"))
  (all-shortest-paths-from-to num-keypad-next-dist [3 2] (num->pos "0"))
  (get-all-seq-to-type-a-char-on-num-keypad "0" [[3 2] ""])
  ;
  )

;; (defn dir->c->dir
;;   "Receives a
;;    - character to type
;;    - and the starting position on the keypad
;;    and returns
;;    - a string of one of the shorterst sequences to perform on the directional keypad
;;    - the final position on the keypad.
;;    e.g. (type-on-numeric-keypd 0) returns <A"
;;   [to-type start]
;;   (str (path-from-to dir-keypad-next start (position-of-char-on-dir-keypad to-type)) "A"))

(defn dir->c->dir
  [to-type start]
  (map (fn [s] (str s "A"))
       (all-shortest-paths-from-to dir-keypad-next-dist start (position-of-char-on-dir-keypad to-type))))

(comment
  (dir->c->dir "<" dir-keypad-start)
  ;
  )

;; (defn dir->dir
;;   [str-to-type start]
;;   (loop [str-to-type str-to-type
;;          start start
;;          seq-cmds ""]
;;     (if (empty? str-to-type)
;;       seq-cmds
;;       (let [to-type (str (first str-to-type))
;;             seq-cmd (dir->c->dir to-type start)]
;;         (recur (rest str-to-type)
;;                (position-on-dir-keypad to-type)
;;                (str seq-cmds seq-cmd))))))

(defn dir->dir
  [str-to-type start]
  (letfn [(dir->dir-rec [str-to-type start seq-cmd]
            ;; (println "Str to type" str-to-type "Start" start "Seq-cmd" seq-cmd)
            (if (empty? str-to-type)
              seq-cmd
              (let [to-type (str (first str-to-type))
                    ;; _ (println "To type" to-type "Start" start)
                    seq-cmds (dir->c->dir
                              to-type start)]
                ;; (println "To type" to-type "Seq-cmds" seq-cmds)
                (let [cls (flatten (mapv (fn [sq]
                                           (dir->dir-rec (subs str-to-type 1)
                                                         [(position-on-dir-keypad to-type) ""] sq))
                                         seq-cmds))]
                  ;; (prn "Cls" cls "Seq-cmd" seq-cmd)
                  ;; (prn "returning " (mapv (fn [s] (str seq-cmd s)) cls))
                  (mapv (fn [s] (str seq-cmd s)) cls)))))]

    (dir->dir-rec str-to-type start "")))

(is (= (count "v<<A>>^A<A>AvA<^AA>A<vAAA>^A")
       (count (dir->dir
               (num->dir "029A" num-keypad-start)
               dir-keypad-start))))

(comment
  (set (map count (flatten (map (fn [w]
                                  (dir->dir w dir-keypad-start))
                                (num->dir "029A" num-keypad-start)))))

  (let [cls (flatten (map (fn [w]
                            (dir->dir w dir-keypad-start))
                          (num->dir "029A" num-keypad-start)))
        m (apply min (map count cls))
        clsf (filter (fn [c] (= m (count c))) cls)
        csdfd (flatten (map (fn [ww]
                              (dir->dir ww dir-keypad-start))
                            clsf))
        m (apply min (map count csdfd))
        csdfd (filter (fn [c] (= m (count c))) csdfd)])

  ;
  )

(comment
  (def str-to-type "029A")
  (def seq-dir (num->dir "029A" num-keypad-start))
  (dir->dir seq-dir dir-keypad-start)

  (dir->dir "<" dir-keypad-start)
  (dir->dir "<A" dir-keypad-start)
  ;
  )

;; (defn type-on-num-via-dir
;;   [to-type]
;;   (let [door-dir (num->dir to-type num-keypad-start)
;;         dir1 (dir->dir door-dir dir-keypad-start)
;;         dir2 (dir->dir dir1 dir-keypad-start)]
;;     (println "Door dir" door-dir)
;;     (println "Dir 1" dir1)
;;     (println "Dir 2" dir2)
;;     dir2))


(defn count-type-on-num-via-dir
  [to-type]
  (let [cls (flatten (map (fn [w]
                            (dir->dir w dir-keypad-start))
                          (num->dir to-type num-keypad-start)))
        m (apply min (map count cls))
        clsf (filter (fn [c] (= m (count c))) cls)
        csdfd (flatten (map (fn [ww]
                              (dir->dir ww dir-keypad-start))
                            clsf))
        m (apply min (map count csdfd))
        csdfd (filter (fn [c] (= m (count c))) csdfd)]
    (count (first csdfd))))


(comment
  (def code "029A")
  (str->nums code)
  (count-type-on-num-via-dir code)
  ;
  )

(defn complexity
  [code]
  (* (count-type-on-num-via-dir code)
     (first (str->nums code))))

(println (apply + (map complexity codes)))
;; 296296 too high
;; 126384 too low
