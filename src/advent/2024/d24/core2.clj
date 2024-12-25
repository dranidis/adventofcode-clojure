(ns advent.2024.d24.core2
  (:require
   [advent.util :refer [bin->long]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.string :as str]))

(def example? false)

(def example "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02")

(def input (if example? example (slurp "src/advent/2024/d24/input.txt")))

(def sections (-> input (str/split #"\n\n")))
(def wires
  (->> sections first (str/split-lines)
       (map #(let [[w v] (str/split % #": ")]
               [w (parse-long v)]))
       (into {})))

(def gates
  (->> sections second (str/split-lines)
       (map #(let [[a op b _ o] (str/split % #" ")]
               [o op a b]))
       (reduce (fn [acc [o op a b]]
                 (conj acc {:i1 a :i2 b :op op :o o}))
               [])))


(let [[{:keys [i1 i2 op o] :as gate} p]
      (peek (into (priority-map) (map (fn [g] [g 0]) gates)))]
  [i1 i2 op o gate p])

(defn evaluate
  [wires]
  (loop [ws wires
         gs (into (priority-map) (map (fn [g] [g 1]) gates))]
    (if-let [[{:keys [i1 i2 op o] :as gate} p] (peek gs)]
      (if (and (contains? ws i1) (contains? ws i2))
        (let [v1 (get ws i1)
              v2 (get ws i2)
              v (case op "AND" (and v1 v2) "OR" (or v1 v2) "XOR" (bit-xor v1 v2))]
          (recur (assoc ws o v) (pop gs)))
        (recur ws (assoc gs gate (inc p))))
      ws)))

(defn answer-1 [] (->> (evaluate wires)
                       (into [])
                       (filter (fn [[w _]] (str/starts-with? w "z")))
                       sort reverse (map second) bin->long))

(println (answer-1))








