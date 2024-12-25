(ns advent.2024.d24.core2
  (:require
   [advent.util :refer [bin-vec->long]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.string :as str]))

(def example? true)

(def example "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(def input (if example? example (slurp "src/advent/2024/d24/input.txt")))
bin-vec->long
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
                       sort reverse (map second)
                      ;;  bin-vec->long
                       ))

(println (answer-1))








