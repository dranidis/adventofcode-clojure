(ns advent.2019.d13.core
  (:require
   [advent.2019.d9.core :refer [run-int-code-computer-state state]]
   [advent.util :refer [draw-grid-condenced grid-2d set-grid str->nums]]))

(def instructions (str->nums (slurp "src/advent/2019/d13/input.txt")))

(println "ANS 1: " (->> (run-int-code-computer-state (state instructions [] [] 0 0))
                        (partition 3)
                        (filter #(= 2 (nth % 2)))
                        count))

(defn- draw [display]
  (let [fil-tiles (fn [n]
                    (->> display (into [])
                         (remove #(= [0 -1] (first %)))
                         (filter #(= n (second %)))
                         (map first)))
        score (get display [0 -1])]
    (println "SCORE: " score)
    (-> (grid-2d 23 43 \.)
        (set-grid (fil-tiles 1) \#)
        (set-grid (fil-tiles 2) \@)
        (set-grid (fil-tiles 0) " ")
        (set-grid (fil-tiles 3) \=)
        (set-grid (fil-tiles 4) \o)
        (draw-grid-condenced))))

(defn- piece [r n]
  (->> r (into [])
       (filter #(= n (second %)))
       first
       first
       second))

(defn- -main [& _]
  (loop [state (state (assoc instructions 0 2) [] [] 0 0)
         display {}]
    (let [r (run-int-code-computer-state state)
          out (if (vector? r) r (:out r))
          display (reduce (fn [a [x y t]] (assoc a [y x] t))
                          display
                          (partition 3 out))
          _ (draw display)
          mv (let [b (piece display 4)
                   p (piece display 3)]
               (cond (> b p) 1 (= b p) 0 :else -1))
          ;; mv (parse-long (read-line)) ;; MANUAL PLAY
          ]
      (if (vector? r)
        (get display [0 -1])
        (recur (assoc r :in [mv]) display)))))