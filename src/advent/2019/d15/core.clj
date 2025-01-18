(ns advent.2019.d15.core
  (:require
   [advent.2019.d9.core :refer [run-int-code-computer-state state]]
   [advent.dijkstra :refer [dijkstra-shortest-distance-to-end-pred]]
   [advent.util :refer [draw-grid grid-2d set-grid str->nums]]
   [clojure.term.colors :as colors]))

(def instructions (->> (slurp "src/advent/2019/d15/input.txt") str->nums))

(def init-state (state instructions [] [] 0 0))

(defn mov [d s]
  (run-int-code-computer-state (assoc s :in [d])))

(defn- pos-of [pos mv]
  (mapv + pos (nth [[-1 0] [1 0] [0 -1] [0 1]] (dec mv))))

(defn- right-direction [dir not-walls-d]
  (let [s (set not-walls-d)]
    (case dir
      4 (first (filter #(s %) [1 4 2 3]))
      2 (first (filter #(s %) [4 2 3 1]))
      3 (first (filter #(s %) [2 3 1 4]))
      1 (first (filter #(s %) [3 1 4 2])))))

(defn- decide-mv [walls pos dir]
  (let [not-walls-d (for [d [1 2 3 4]
                          :let [n-pos (pos-of pos d)]
                          :when (not (walls n-pos))]
                      d)]
    (if (nil? dir)
      (last not-walls-d)
      (right-direction dir not-walls-d))))

(defn draw [walls visited pos goal expand]
  (let [tiles (vec (concat walls visited))
        tiles (if (some? pos) (conj tiles pos) tiles)
        ;; minr (apply min (mapv first tiles))
        ;; minc (apply min (mapv second tiles))
        ;; maxr (apply max (mapv first tiles))
        ;; maxc (apply max (mapv second tiles))
        minr -21
        minc -21
        ;; rows (inc (- maxr minr))
        ;; cols (inc (- maxc minc))
        rows 41
        cols 41
        translate (fn [d]
                    (fn [p] (mapv - p d)))]
    (-> (grid-2d rows cols " ")
        (set-grid (mapv (translate [minr minc]) tiles) (colors/cyan \#))
        (set-grid (mapv (translate [minr minc]) visited) \.)
        (set-grid (if (some? pos) [((translate [minr minc]) pos)] []) (colors/magenta \o))
        (set-grid [((translate [minr minc]) [0 0])] (colors/green \S))
        (set-grid (if (some? goal) [((translate [minr minc]) goal)] []) (colors/red \E))
        (set-grid (if (some? expand) (mapv (translate [minr minc]) expand) []) (colors/on-white \O))
        (draw-grid))))

(def maz (loop [s init-state
                walls #{}
                pos [0 0]
                dir nil
                visited #{pos}
                cnt 0
                goal nil]
           (println cnt)
           (draw walls visited pos goal nil)
           (if (and (some? goal) (= pos [0 0]))
             [goal walls visited]
             (let [mv (decide-mv walls pos dir)
                   new-s (mov mv s)
                   new-pos (pos-of pos mv)]
               (case (first (:out new-s))
                 0 (recur new-s (conj walls new-pos) pos dir visited (inc cnt) goal)
                 1 (recur new-s walls new-pos mv (conj visited new-pos) (inc cnt) goal)
                 2 (recur new-s walls new-pos mv (conj visited new-pos) (inc cnt) new-pos))))))

(defn- neighbors [maz]
  (let [[_ _ v] maz]
    (fn [pos]
      (for [d [1 2 3 4]
            :let [n-pos (pos-of pos d)]
            :when (v n-pos)]
        [1 n-pos]))))

(def answer-1 (dijkstra-shortest-distance-to-end-pred
               [0 0]
               (fn [n] (= n (first maz)))
               (neighbors maz)))

(def answer-2
  (let [[g w _] maz]
    (loop [expand [g]
           visited #{g}
           tim 0]
      (println tim)
      (draw w visited nil g expand)
      (let [expand (for [d [1 2 3 4]
                         e expand
                         :let [n-pos (pos-of e d)]
                         :when (and (not (w n-pos))
                                    (not (visited n-pos)))]
                     n-pos)]
        (if (empty? expand)
          tim
          (recur expand (apply conj visited expand) (inc tim)))))))

(defn- -main [& _]
  (println "ANS 1:" answer-1)
  (println "ANS 2:" answer-2))