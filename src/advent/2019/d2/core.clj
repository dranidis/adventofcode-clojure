(ns advent.2019.d2.core
  (:require [advent.util :refer [str->nums]]))

(def instructions (-> (slurp "src/advent/2019/d2/input.txt")
                      str->nums))

(defn run-instructions [v1 v2]
  (loop [instructions (assoc instructions 1 v1 2 v2)
         cnt 0]
    (let [ins (nth instructions cnt)]
      (if (= ins 99)
        (first instructions)
        (let [nums (mapv #(->> (nth instructions (+ cnt %))
                               (get instructions))
                         [1 2])
              at (nth instructions (+ cnt 3))
              instructions (-> instructions
                               (assoc at (case ins
                                           1 (apply + nums)
                                           (apply * nums))))]
          (recur instructions (+ 4 cnt)))))))

(println "ANS 1: " (run-instructions 12 2))

(println "ANS 2: " (first (for [v1 (range 0 145)
                                v2 (range 0 145)
                                :when (= 19690720 (run-instructions v1 v2))]
                            (+ (* 100 v1) v2))))