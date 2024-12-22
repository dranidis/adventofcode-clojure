(ns advent.2024.d11.core
  (:require
   [clojure.string :as str]))

(def input (slurp "src/advent/2024/d11/input.txt"))

(def stones (mapv parse-long (re-seq #"\d+" input)))

(def stones-map
  "Mapping of stones to their count"
  (reduce (fn [acc s]
            (assoc acc s (inc (get acc s 0))))
          {}
          stones))

(defn- change-stone
  [s]
  (cond
    (zero? s) [1]

    (even? (count (str s)))
    (mapv (fn [digits] (parse-long (str/join "" digits)))
          (split-at (/ (count (str s)) 2) (str s)))

    :else [(* s 2024)]))

(defn increase-by-freq
  [stones-map new-stone-key freq]
  (update stones-map new-stone-key (fnil + 0) freq))

(defn stones-map-change
  [stones-map]
  (loop [keys (keys stones-map)
         values-to-add-at-the-end {}
         new-stones-map stones-map]
    (if (empty? keys)
      (reduce-kv (fn [acc k v]
                   (assoc acc k v))
                 new-stones-map
                 values-to-add-at-the-end)

      (let [key (first keys)
            values-to-add-sync
            (reduce (fn [acc new-stone-key]
                      (increase-by-freq acc new-stone-key (get stones-map key)))
                    values-to-add-at-the-end
                    (change-stone key))]

        (recur (rest keys)
               values-to-add-sync
               (dissoc new-stones-map key))))))

(defn stones-map-after-times
  [times]
  (->> stones-map (iterate stones-map-change) (drop times) first))

(println "Day 11, Answer 1" (->> (stones-map-after-times 25) vals (apply +)))
(println "Day 11, Answer 2" (->> (stones-map-after-times 75) vals (apply +)))
