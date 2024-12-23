(ns advent.2024.d23.core
  (:require
   [advent.graph :refer [maximum-clique]]
   [clojure.string :as str]))

(def example? false)

(def example "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(def input (if example? example (slurp "src/advent/2024/d23/input.txt")))

(def edges (->> input
                (str/split-lines)
                (mapv #(str/split % #"\-"))
                (reduce (fn [c-set [c1 c2]]
                          (conj c-set [c1 c2] [c2 c1]))
                        #{})))

(def answer-1
  (-> (for [[c1 c2] edges
            :when (str/starts-with? c1 "t")
            [c2a c3] edges
            :when (and (= c2 c2a)
                       (not= c1 c3)
                       (edges [c1 c3]))]
        #{c1 c2 c3})
      set
      count))

;; PART 2

(defn build-adjacency-list
  [edges]
  (->> edges
       (reduce (fn [adj [c1 c2]]
                 (update adj c1 (fnil #(conj % c2) #{})))
               {})))

(def answer-2
  (->> edges
       (build-adjacency-list)
       (maximum-clique)
       sort
       (str/join ",")))

(defn- -main [& _]
  (println "Day 23, Part 1:" answer-1)
  (println "Day 23, Part 2:" answer-2))
