(ns advent.2024.perf
  (:require
   [clojure.core.reducers :as reducers]
   [criterium.core :refer [quick-bench]]))


;; (defn fibonacci [n]
;;   (if (<= n 1)
;;     n
;;     (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; ;; (fibonacci 42)

;; (quick-bench (reduce + (map fibonacci (range 30))))

;; (quick-bench (reducers/fold + (reducers/map fibonacci (range 30))))

;; (quick-bench (reducers/fold + (reducers/map fibonacci (vec (range 30)))))
(def coll (range 1e6))

(quick-bench (last coll))
(quick-bench (nth coll (dec (count coll))))
(quick-bench (peek (vec coll)))
 