(ns advent.2019.d7.core
  (:require
   [advent.2019.d9.core :refer [run-int-code-computer]]
   [advent.util :refer [str->nums]]
   [clojure.math.combinatorics :as combo]))


(def example? false)

(def example "")

(def input (if example? example (slurp "src/advent/2019/d7/input.txt")))

(def instructions (->> input str->nums))

(println "ANS 1: "
         (->> (mapv
               (fn [[p1 p2 p3 p4 p5]]
                 (let [out-s (run-int-code-computer instructions 0 [p1 0])
                       out-s (run-int-code-computer instructions 0 [p2 out-s])
                       out-s (run-int-code-computer instructions 0 [p3 out-s])
                       out-s (run-int-code-computer instructions 0 [p4 out-s])
                       out-s (run-int-code-computer instructions 0 [p5 out-s])]
                   out-s))
               (combo/permutations (range 5)))
              (apply max)))

(defn loop-amp [instructions p1 p2 p3 p4 p5]
  (let [[ins-1 _ cnt-1]
        (run-int-code-computer instructions 0 [p1])
        [ins-2 _ cnt-2]
        (run-int-code-computer instructions 0 [p2])
        [ins-3 _ cnt-3]
        (run-int-code-computer instructions 0 [p3])
        [ins-4 _ cnt-4]
        (run-int-code-computer instructions 0 [p4])
        [ins-5 _ cnt-5]
        (run-int-code-computer instructions 0 [p5])]
    (loop [instructions [ins-1 ins-2 ins-3 ins-4 ins-5]
           cnts [cnt-1 cnt-2 cnt-3 cnt-4 cnt-5]
           out-s 0]
      (let [res (run-int-code-computer (nth instructions 0) (nth cnts 0) [out-s])]
        (if (number? res)
          (let [out-s
                (run-int-code-computer (nth instructions 1) (nth cnts 1) [res])
                out-s
                (run-int-code-computer (nth instructions 2) (nth cnts 2) [out-s])
                out-s
                (run-int-code-computer (nth instructions 3) (nth cnts 3) [out-s])
                out-s
                (run-int-code-computer (nth instructions 4) (nth cnts 4) [out-s])]
            out-s)
          (let [[ins-1 out-s cnt-1]
                (run-int-code-computer (nth instructions 0) (nth cnts 0) [out-s])
                [ins-2 out-s cnt-2]
                (run-int-code-computer (nth instructions 1) (nth cnts 1) [out-s])
                [ins-3 out-s cnt-3]
                (run-int-code-computer (nth instructions 2) (nth cnts 2) [out-s])
                [ins-4 out-s cnt-4]
                (run-int-code-computer (nth instructions 3) (nth cnts 3) [out-s])
                [ins-5 out-s cnt-5]
                (run-int-code-computer (nth instructions 4) (nth cnts 4) [out-s])]
            (recur [ins-1 ins-2 ins-3 ins-4 ins-5]
                   [cnt-1 cnt-2 cnt-3 cnt-4 cnt-5]
                   out-s)))))))

(println "ANS 2: "
         (->> (mapv
               (fn [[p1 p2 p3 p4 p5]]
                 (loop-amp instructions p1 p2 p3 p4 p5))
               (combo/permutations (range 5 10)))
              (apply max)))
