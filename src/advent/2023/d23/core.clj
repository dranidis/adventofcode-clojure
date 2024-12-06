(ns advent.2023.d23.core
  (:require
   [advent.util :refer [str->2D]]))

(def input "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

;; (def input
;;   "#.####
;;    #.>..#
;;    #.##v#
;;    #....#
;;    #.####")

(defn get-map
  "Returns [rows cols [sr sc] m] where m is list of tiles"
  [input]
  (let [pi (str->2D input)
        rows (count pi)
        cols (count (first pi))
        m (mapv vec (partition cols (vec (for [r (range rows)
                                               c (range cols)
                                               :let [tile (get-in pi [r c])]]
                                           tile))))
        sc  (.indexOf (first m) ".")]
    [rows cols [0 sc] m]))


(get-map input)
;; (let [[rows cols s m] (get-map input)]
;;   (loop [[r c] s
;;          seen #{s}
;;          path [s]
;;          t 0]
;;     (println t [r c] (get-in m [r c]))
;;     (if (or (= t 200)
;;             (= r (dec rows)))
;;       path
;;       (let [new-rcs (case (get-in m [r c])
;;                       ">" [[r (inc c)]]
;;                       "<" [[r (dec c)]]
;;                       "^" [[(dec r) c]]
;;                       "v" [[(inc r) c]]
;;                       (vec (for [[dr dc] [[0 1] [1 0] [0 -1] [-1 0]]
;;                                  :let [nr (+ r dr)
;;                                        nc (+ c dc)]
;;                                  :when (and (<= 0 nr (dec rows))
;;                                             (<= 0 nc (dec cols))
;;                                             (not= "#" (get-in m [nr nc]))
;;                                             (not (seen [nr nc])))]
;;                              [nr nc])))
;;             ;; _ (println new-rcs)
;;             new-rc (first new-rcs)]
;;         (recur new-rc (conj seen new-rc) (conj path new-rc) (inc t))))))

(defn new-positions [rows cols m seen [r c]]
  (case (get-in m [r c])
    ">" (remove (fn [p] (seen p)) [[r (inc c)]])
    "<" (remove (fn [p] (seen p)) [[r (dec c)]])
    "^" (remove (fn [p] (seen p)) [[(dec r) c]])
    "v" (remove (fn [p] (seen p)) [[(inc r) c]])
    (vec (for [[dr dc] [[0 1] [1 0] [0 -1] [-1 0]]
               :let [nr (+ r dr)
                     nc (+ c dc)]
               :when (and (<= 0 nr (dec rows))
                          (<= 0 nc (dec cols))
                          (not= "#" (get-in m [nr nc]))
                          (not (seen [nr nc])))]
           [nr nc]))))

(comment
  (let [[rows cols s m] (get-map input)]
    (loop [[r c] s
           seen #{s}
           path [s]
           t 0]
      (println t [r c] (get-in m [r c]))
      (if (or (= t 200)
              (= r (dec rows)))
        path
        (let [new-rcs (new-positions rows cols m seen [r c])
              ;; _ (println new-rcs)
              new-rc (first new-rcs)]
          (recur new-rc (conj seen new-rc) (conj path new-rc) (inc t))))))
  ;
  )

;; (defn my-max [coll]
;;   (if (empty? coll) 0
;;       (apply max coll)))

;; (defn walk [input]
;;   (let [[rows cols s m] (get-map input)]
;;     (println rows cols s)
;;     ;; (pprint/pprint m)
;;     (letfn [(dfs [p path]
;;               (if (= (first p) (dec rows))
;;                 (do
;;                   (println (count path) path)
;;                   (count path))
;;                 (let [next-ps (new-positions rows cols m (set path) p)]
;;                   (println next-ps)
;;                   (my-max (map (fn [np]
;;                                  (dfs np (conj path np)))
;;                                next-ps)))))]
;;       (dfs s [s]))))

(defn walk-loop [input]
  (let [[rows cols s m] (get-map input)]
    ;; (println rows cols s)
      ;; (pprint/pprint m)
    (letfn [(dfs-loop [p path]
              (loop [stack [[p path]]
                     max-v  0
                     max-path []]
                ;; (prn (count stack) max-v)
                (if (empty? stack)
                  max-v
                  (let [[p path] (peek stack)]
                    ;; (prn "D" p path)
                    (if (= (first p) (dec rows))
                      (let [[max-v max-path]
                            (if (> (dec (count path)) max-v)
                              [(dec (count path)) path]
                              [max-v max-path])]
                        (prn max-v (dec (count path)) path)
                        (recur (pop stack) max-v max-path))
                      (let [next-ps (new-positions rows cols m (set path) p)
                            next-ps-paths (map (fn [np]
                                                 [np (conj path np)])
                                               next-ps)]
                        (recur (apply conj (pop stack) next-ps-paths) max-v max-path)))))))]
      (dfs-loop s [s]))))

(defn -main [& _]
  ;; (time (println "Day 1, Part 1:" (answer-1 input)))
  ;; (time (println "Day 1, Part 1:" (answer-1 (slurp "src/advent/2023/d21/input.txt"))))
  (println (walk-loop input))

;;   (map (fn [r] (apply str r)) (reduce (fn [m [r c]]
;;             (assoc-in m [r c] "O")) 
;;         (nth (get-map input) 3)
;;         (second (walk-loop input))))



  (println (walk-loop (slurp "src/advent/2023/d23/input.txt")))
   ; 
  )


