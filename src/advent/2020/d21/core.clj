(ns advent.2020.d21.core
  (:require
   [advent.util :refer [in-vector?]]
   [clojure.string :as str]
   [clojure.set :as set]))

(def foods
  (->> (str/split-lines (slurp "src/advent/2020/d21/input.txt"))
       (mapv (fn [line]
               (let [[f r] (str/split line #" \(contains ")]
                 [(vec (re-seq #"\w+" f))
                  (vec (re-seq #"\w+" r))])))))

(def allergens
  (->> foods
       (mapcat second)
       set))

(def candidates-for-allergen
  (->> allergens
       (map (fn [a]
              [a (apply set/intersection
                        (reduce (fn [acc [ing ia]]
                                  (if (in-vector? ia a)
                                    (conj acc (set ing))
                                    acc))
                                []
                                foods))]))))

(defn singles [d]
  (->> d
       (filter #(= 1 (count (second %))))))

(defn allergen-singletons [candidates-for-allergen]
  (loop [candidates-for-allergen candidates-for-allergen]
    ;; (prn all)
    (if (every? (fn [[a ing-set]] (= 1 (count ing-set)))
                candidates-for-allergen)
      candidates-for-allergen
      (let [s (mapcat second (singles candidates-for-allergen))]

        (recur (reduce (fn [acc [a ing-set]]
                         (conj acc [a
                                    (if (= 1 (count ing-set))
                                      ing-set
                                      (apply disj ing-set s))]))
                       []
                       candidates-for-allergen))))))

(->> foods
     (mapcat first)
     (remove (fn [x]
               ((->> (allergen-singletons candidates-for-allergen)
                     (mapcat second)
                     set) x)))
     count
     (println "ANS 1: "))

(->> (allergen-singletons candidates-for-allergen)
     (sort)
     (mapcat second)
     (str/join ",")
     (println "ANS 2: "))




