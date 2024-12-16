(ns advent.dijkstra-test
  (:require
   [advent.dijkstra :as d]
   [clojure.test :refer [deftest is]]))

(deftest dijkstra-shortest-distances-test
  (let [neighbors (fn [node]
                    (case node
                      :A [[1 :B] [1 :C]]
                      :B [[1 :D]]
                      :C [[1 :D]]
                      :D []))
        start :A
        end :D]

    (is (= {:A [0 nil], :B [1 :A], :C [1 :A], :D [2 :B]}
           (d/dijkstra-shortest-distances-pred start neighbors)))

    (is (= {:A [0 #{}], :B [1 #{:A}], :C [1 #{:A}], :D [2 #{:B :C}]}
           (d/dijkstra-shortest-distances-pred-set start neighbors)))

    (is (= [:A :B :D]
           (d/shortest-path-from-to neighbors start end)))

    (is (= '([:A :B :D] [:A :C :D])
           (d/all-shortest-paths-from-to neighbors start end)))))