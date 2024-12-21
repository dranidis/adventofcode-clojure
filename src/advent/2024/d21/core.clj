(ns advent.2024.d21.core
  (:require
   [advent.2023.d17.priority-queue :refer [add-with-priority! extract-min!
                                           make-priority-queue!]]
   [advent.dijkstra :refer [all-shortest-paths-from-to]]
   [advent.util :refer [coords-of-symbol str->2D str->nums]]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(def example? true)
(def example "029A
980A
179A
456A
379A")

(def input (if example? example (slurp "src/advent/2024/d21/input.txt")))
(defn parse-codes [input]
  (str/split-lines input))
(def codes (parse-codes input))

;; DIJKSTRA
(defn- shortest-path-from-to
  [D start to]
  (println "Shortest path from" start "to" to "D" D)
  (vec (loop [to to
              path (list to)]
         (if (= to start)
           path
           (let [to (second (get D to))]
             (recur to (conj path to)))))))

(defn path-from-to
  [neighbors start end]
  ;; Q is a priority queue of [distance previous-node node]
  ;; D is a map from node to [distance previous-node]
  (let [Q (add-with-priority! (make-priority-queue!
                               (fn [[d _] [d2 _]] (< d d2))) [0 nil start ""])
        D {}]
    (loop [Q Q
           D D]
      (if-not (empty? Q)
        (let [[dist prev node cmdstr] (extract-min! Q)]
          ;; (println "Current node" node "distance" dist "queue" Q "prev" prev)
          ;; (println "Distances" D)

          ;; If we have already visited the node, skip it.
          (if (= node end)
            cmdstr
            ;; (shortest-path-from-to (assoc D node [dist prev]) start end)
            (if (get D node)
              (recur Q D)
              (let [D (assoc D node [dist prev])
                    Q (apply add-with-priority! Q
                             (map (fn [[n cmd]] [(inc dist) node n (str cmdstr cmd)])
                                  (neighbors node)))]
                (recur Q D)))))
        D))))
;; 

(def num-keypad
  (str->2D
   "789
    456
    123
    #0A"))

(defn num-keypad-next
  [[r c]]
  ;;  <
  (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
        :let [nr (+ r dr)
              nc (+ c dc)
              cell (get-in num-keypad [nr nc])]
        :when (and (some? cell)
                   (not= cell "#"))]
    [[nr nc] cmd]))

(defn num-keypad-next-dist
  [[r c]]
  ;;  <
  [1 (num-keypad-next [r c])])

(defn position-of-char-on-num-keypad
  [char]
  (first (coords-of-symbol num-keypad char)))

(def position-on-num-keypad
  (assoc (zipmap (map str (range 10))
                 (map position-of-char-on-num-keypad (map str (range 10))))
         "A" [3 2]))

(def num-keypad-start (position-on-num-keypad "A"))

;; 
;; 
;; 

(def dir-keypad
  (str->2D
   "#^A
    <v>"))


(defn dir-keypad-next
  [[r c]]
  ;;  <
  (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
        :let [nr (+ r dr)
              nc (+ c dc)
              cell (get-in dir-keypad [nr nc])]
        :when (and (some? cell)
                   (not= cell "#"))]
    [[nr nc] cmd]))

(defn position-of-char-on-dir-keypad
  [char]
  (first (coords-of-symbol dir-keypad char)))

(def position-on-dir-keypad
  (assoc (zipmap ["^" "v" "<" ">"]
                 (map position-of-char-on-dir-keypad ["^" "v" "<" ">"]))
         "A" [0 2]))

(def dir-keypad-start (position-on-dir-keypad "A"))





(defn get-seq-to-type-a-char-on-num-keypad
  "Receives a 
   - character to type
   - and the starting position on the keypad 
   and returns 
   - a string of one of the shorterst sequences to perform on the directional keypad
   - the final position on the keypad.
   e.g. (type-on-numeric-keypd 0) returns <A"
  [to-type start]
  (str (path-from-to num-keypad-next start (position-of-char-on-num-keypad to-type)) "A"))

(defn get-all-seq-to-type-a-char-on-num-keypad
  [to-type start]
  (str (all-shortest-paths-from-to num-keypad-next-dist start (position-of-char-on-num-keypad to-type)) "A"))


(comment
  (get-seq-to-type-a-char-on-num-keypad "0" num-keypad-start)
;
  )

(defn num->dir
  [str-to-type start]
  (loop [str-to-type str-to-type
         start start
         seq-cmds ""]
    (if (empty? str-to-type)
      seq-cmds
      (let [to-type (str (first str-to-type))
            seq-cmd (get-seq-to-type-a-char-on-num-keypad to-type start)]
        (recur (rest str-to-type)
               (position-on-num-keypad to-type)
               (str seq-cmds seq-cmd))))))

(comment
  (def str-to-type "029A")

  (num->dir "029A" num-keypad-start)
  ;
  )

(is (some? (#{"<A^A>^^AvvvA" "<A^A^>^AvvvA" "<A^A^^>AvvvA"}
            (num->dir "029A" num-keypad-start))))



(defn dir->c->dir
  "Receives a 
   - character to type
   - and the starting position on the keypad 
   and returns 
   - a string of one of the shorterst sequences to perform on the directional keypad
   - the final position on the keypad.
   e.g. (type-on-numeric-keypd 0) returns <A"
  [to-type start]
  (str (path-from-to dir-keypad-next start (position-of-char-on-dir-keypad to-type)) "A"))

(defn dir->dir
  [str-to-type start]
  (loop [str-to-type str-to-type
         start start
         seq-cmds ""]
    (if (empty? str-to-type)
      seq-cmds
      (let [to-type (str (first str-to-type))
            seq-cmd (dir->c->dir to-type start)]
        (recur (rest str-to-type)
               (position-on-dir-keypad to-type)
               (str seq-cmds seq-cmd))))))

(is (= (count "v<<A>>^A<A>AvA<^AA>A<vAAA>^A")
       (count (dir->dir
               (num->dir "029A" num-keypad-start)
               dir-keypad-start))))
(comment
  (def str-to-type "029A")
  (def seq-dir (num->dir "029A" num-keypad-start))
  (dir->dir seq-dir dir-keypad-start)

  (dir->dir "<" dir-keypad-start)
  (dir->dir "<A" dir-keypad-start)
  ;
  )

(defn type-on-num-via-dir
  [to-type]
  (let [door-dir (num->dir to-type num-keypad-start)
        dir1 (dir->dir door-dir dir-keypad-start)
        dir2 (dir->dir dir1 dir-keypad-start)]
    (println "Door dir" door-dir)
    (println "Dir 1" dir1)
    (println "Dir 2" dir2)
    dir2))

(comment
  (def code "029A")
  (str->nums code)
  (count (type-on-num-via-dir code))
  ;
  )

(defn complexity
  [code]
  (* (count (type-on-num-via-dir code))
     (first (str->nums code))))

(println (apply + (map complexity codes)))
;; 296296 too high

(comment

  ;
  )
;; 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
;; "v<A<AA>>^AvAA^<A>Av<<A>>^AvA^Av<<A>>^Av<A>A^A<Av<A>>^AvA^Av<A<A>>^AAAvA^<A>A"


;; (def answer-1 nil)
;; (def answer-2 nil)
;; (defn- -main [& _]
;;   (println "Day XX, Part 1:" answer-1)
;;   (println "Day XX, Part 2:" answer-2))

;; (-main)
