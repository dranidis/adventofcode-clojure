(ns advent.2021.d14.core
  (:require
   [advent.util :refer [in-vector?]]
   [clojure.string :as str]))

;; Solved with Side Effects
(def map-atom (atom {}))

;; (comment
(def input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")
 ;
  ;; )

(def input (slurp "src/advent/2021/d14/input.txt"))

(defn parse-rule
  [rule]
  (vec (re-seq #"\w" rule)))

(defn parse-parts
  [input]
  (let [sections (str/split input #"\n\n")
        template (first sections)
        rules (mapv parse-rule (str/split-lines (second sections)))]
    [template rules]))

(def parsed (parse-parts input))
(def template (first parsed))

(defn inc-nil [v] (if (nil? v) (bigint 1) (inc v)))

(defn reset-atom! []
  (reset! map-atom {})
  (reduce (fn [_ c] (swap! map-atom update c inc-nil)) nil (vec template)))

(def rules (second parsed))

(def template-pairs
  (let [partitioned (partition 2 1 template)]
    (reduce (fn [acc [c1 c2]]
              (update acc (str c1 c2) inc-nil))
            {}
            partitioned)))

(def rules-pairs
  (reduce (fn [acc [r1 r2 r3]]
            (assoc acc (str r1 r2) r3))
          {}
          rules))

(defn apply-rules-pairs
  [rules-pairs template-pairs]
  (loop [ks (keys template-pairs)
         updates {}]
    (if (empty? ks)
      ;; all updates must be applied at once
      (reduce-kv
       (fn [acc k v]
         (update acc k (fn [ov] (if (nil? ov) (bigint v) (+ ov v)))))
       template-pairs
       updates)
      (let [pair (first ks)
            times (get template-pairs pair)
            r (get rules-pairs pair)
            xr (str (first pair) r)
            ry (str r (last pair))
            _ (swap! map-atom update (first (vec r)) (fn [v] (if (nil? v) (bigint times) (+ v times))))
            updates (-> updates
                        (update pair (fn [v] (if (nil? v) (- times) (- v times))))
                        (update xr (fn [v] (if (nil? v) (bigint times) (+ v times))))
                        (update ry (fn [v] (if (nil? v) (bigint times) (+ v times)))))]
        (recur (rest ks) updates)))))

(defn tempate-pairs-after
  [rules-pairs template-pairs n]
  (loop [times 0
         template-pairs template-pairs]
    (if (= times n)
      template-pairs
      (recur (inc times) (apply-rules-pairs rules-pairs template-pairs)))))

;; SIDE EFFECTS
(reset-atom!)
(tempate-pairs-after
 rules-pairs
 template-pairs 10)

(println "Day 14 Answer 1:" (- (apply max (vals @map-atom)) (apply min (vals @map-atom))))

(reset-atom!)
(tempate-pairs-after
 rules-pairs
 template-pairs 40)
@map-atom

(println "Day 14 Answer 2:" (- (apply max (vals @map-atom)) (apply min (vals @map-atom))))




