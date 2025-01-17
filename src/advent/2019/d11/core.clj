(ns advent.2019.d11.core
  (:require
   [advent.2019.d9.core :refer [run-int-code-computer-all]]
   [advent.util :refer [draw-grid-condenced grid-2d set-grid str->nums]]))

(def example? false)

(def example "")

(def input (if example? example (slurp "src/advent/2019/d11/input.txt")))
(def instructions (str->nums input))

(defn- turn [current-dir lr]
  (if (zero? lr)
    (mod (+ 3 current-dir) 4)
    (mod (inc current-dir) 4)))

(def dir-map {0 [-1 0]
              1 [0 1]
              2 [1 0]
              3 [0 -1]})

(defn r [init-white? colors]
  (loop [instr instructions
         cnt 0
         base 0
         white? init-white?
         colors colors
         current [0 0]
         current-dir 0
         visited? #{}]
    (let [current-clr (get colors current 0)
          rc (run-int-code-computer-all instr cnt base [current-clr])
          ;; f (:ins rc)
          ]
      (if (not (vector? rc)) ;; first is instructions map
        (let [[upd-instr outputs new-cnt new-base] (mapv rc [:ins :out :cnt :base])
              _ (assert (and (vector? outputs) (= 2 (count outputs))))
              [paint-color left-of-right] outputs
              _ (assert (and (#{0 1} paint-color) (#{0 1} left-of-right)))
              new-white? (if (zero? paint-color)
                           (disj white? current)
                           (conj white? current))
              new-colors (assoc colors current paint-color)
              new-current-dir (turn current-dir left-of-right)
              _ (assert (#{0 1 2 3} new-current-dir))
              new-current (mapv + current (dir-map new-current-dir))]
          (recur upd-instr
                 new-cnt
                 new-base
                 new-white?
                 new-colors
                 new-current
                 new-current-dir
                 (conj visited? current)))

        [(count colors) colors]))))

(def answer-1 (first (r #{} {[0 0] 0})))

(defn- translate [d]
  (fn [p] (mapv - p d)))

(defn- -main [& _]
  (let [d (second (r #{[0 0]} {[0 0] 1}))
        d (->> d (into []) (filter (fn [[p c]] (= c 1))) (mapv first))
        minr (apply min (mapv first d))
        minc (apply min (mapv second d))
        maxr (apply max (mapv first d))
        maxc (apply max (mapv second d))
        rows (inc (- maxr minr))
        cols (inc (- maxc minc))]
    (println "ANS 1:" answer-1)
    (println "ANS 2:")
    (-> (grid-2d rows cols " ")
        (set-grid (mapv (translate [minr minc]) d) \#)
        (draw-grid-condenced))))