(ns advent.2023.d20-test
  (:require [advent.2023.d20.core :refer [process-signal]]
            [clojure.test :refer [deftest is]]))

(deftest p
  (let [modules {:broadcaster {:name :broadcaster, :type :br, :to #{:c :b :a}},
                 :a {:name :a, :type :ff, :to #{:b}, :state false},
                 :b {:name :b, :type :ff, :to #{:c}, :state false},
                 :c {:name :c, :type :ff, :to #{:inv}, :state false},
                 :inv {:name :inv, :type :conj, :to #{:a}, :state {:c 0}}}

        signals [[nil :broadcaster 0]]]

    (is (= [{:broadcaster {:name :broadcaster, :type :br, :to #{:c :b :a}},
             :a {:name :a, :type :ff, :to #{:b}, :state false},
             :b {:name :b, :type :ff, :to #{:c}, :state false},
             :c {:name :c, :type :ff, :to #{:inv}, :state false},
             :inv {:name :inv, :type :conj, :to #{:a}, :state {:c 0}}}
            [[:broadcaster :c 0] [:broadcaster :b 0] [:broadcaster :a 0]]]
           (reduce (fn [[m s] sig]
                     (let [[mods sigs] (process-signal m sig)]
                       [mods (apply conj s sigs)]))
                   [modules []]
                   signals))))

  (let [[modules signals]
        [{:broadcaster {:name :broadcaster, :type :br, :to #{:c :b :a}},
          :a {:name :a, :type :ff, :to #{:b}, :state false},
          :b {:name :b, :type :ff, :to #{:c}, :state false},
          :c {:name :c, :type :ff, :to #{:inv}, :state false},
          :inv {:name :inv, :type :conj, :to #{:a}, :state {:c 0}}}

         [[:broadcaster :c 0]
          [:broadcaster :b 0]
          [:broadcaster :a 0]]]]

    (is (= [{:broadcaster {:name :broadcaster, :type :br, :to #{:c :b :a}},
             :a {:name :a, :type :ff, :to #{:b}, :state true},
             :b {:name :b, :type :ff, :to #{:c}, :state true},
             :c {:name :c, :type :ff, :to #{:inv}, :state true},
             :inv {:name :inv, :type :conj, :to #{:a}, :state {:c 0}}}
            [[:c :inv 1]
             [:b :c 1]
             [:a :b 1]]]
           (reduce (fn [[m s] sig]
                     (let [[mods sigs] (process-signal m sig)]
                       [mods (apply conj s sigs)]))
                   [modules []]
                   signals))))

  (let [[modules signals]
        [{:broadcaster {:name :broadcaster, :type :br, :to #{:c :b :a}},
          :a {:name :a, :type :ff, :to #{:b}, :state true},
          :b {:name :b, :type :ff, :to #{:c}, :state true},
          :c {:name :c, :type :ff, :to #{:inv}, :state true},
          :inv {:name :inv, :type :conj, :to #{:a}, :state {:c 0}}}

         [[:c :inv 1] [:b :c 1] [:a :b 1]]]]

    (is (= [{:broadcaster {:name :broadcaster, :type :br, :to #{:c :b :a}},
             :a {:name :a, :type :ff, :to #{:b}, :state true},
             :b {:name :b, :type :ff, :to #{:c}, :state true},
             :c {:name :c, :type :ff, :to #{:inv}, :state true},
             :inv {:name :inv, :type :conj, :to #{:a}, :state {:c 1}}}
            [[:inv :a 0]]]
           (reduce (fn [[m s] sig]
                     (let [[mods sigs] (process-signal m sig)]
                       [mods (apply conj s sigs)]))
                   [modules []]
                   signals)))))