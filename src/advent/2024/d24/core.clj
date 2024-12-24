(ns advent.2024.d24.core
  (:require
   [advent.util :refer [bin->long]]
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

;; SECTIONS
(def wire-values-section (first (str/split input #"\n\n")))
(def gates-section (second (str/split input #"\n\n")))

(defn parse-wires [wire-values-section]
  (vec (for [line (str/split-lines wire-values-section)]
         (let [[name v] (str/split line #": ")]
           [name (parse-long v)]))))

(defn parse-gates [gates-section]
  (vec (for [line (str/split-lines gates-section)]
         (let [[in1 g in2 _ o] (str/split line #" ")]
           [in1 g in2 o]))))

(def wires (into {} (parse-wires wire-values-section)))
(def gates (reduce (fn [acc [i1 g i2 o]]
                     (conj acc {:i1 i1 :i2 i2 :op g :o o :v nil :iv1 nil :iv2 nil}))
                   []
                   (parse-gates gates-section)))

(defn operate [gate]
  (if (and (some? (:iv1 gate)) (some? (:iv2 gate)))
    (let [v1 (:iv1 gate)
          v2 (:iv2 gate)]
      (case (:op gate)
        "AND" (assoc gate :v (bit-and v1 v2))
        "OR" (assoc gate :v (bit-or v1 v2))
        "XOR" (assoc gate :v (bit-xor v1 v2))))
    gate))

(def all-wires (reduce (fn [w gate]
                         (-> w
                             ((fn [w] (if (get w (:i1 gate)) w
                                          (assoc w (:i1 gate) nil))))
                             ((fn [w] (if (get w (:i2 gate)) w
                                          (assoc w (:i2 gate) nil))))
                             ((fn [w] (if (get w (:o gate)) w
                                          (assoc w (:o gate) nil))))))
                       wires
                       gates))

(defn update-wg
  [wires gates]
  (loop [wires wires
         gates gates
         updated-gates []]
    (if (empty? gates)
      [wires updated-gates]
      (let [gate (nth gates 0)
            v1 (get wires (:i1 gate))
            v2 (get wires (:i2 gate))
            gate (-> gate
                     (assoc  :iv1 v1 :iv2 v2)
                     operate)
            updated-wires (assoc wires (:o gate) (:v gate))]
        (recur updated-wires (rest gates) (conj updated-gates gate))))))

(defn settled
  [gates after all-wires]
  (loop [all-wires all-wires
         gates gates
         cnt 0]
    (if (or (= cnt after)
            (every? (fn [[w v]] (some? v)) (into [] all-wires)))
      (do (when (= cnt after) (println "TIMEOUT"))
          all-wires)
      (let [[updated-wires updated-gates] (update-wg all-wires gates)]
        (recur updated-wires updated-gates (inc cnt))))))

(def bits (map second (reverse (sort (filter (fn [[w v]] (str/starts-with? w "z"))
                                             (into [] (settled  gates 100 all-wires)))))))

(defn answer-1 [] (->> all-wires
                       (settled gates 10000000)
                       (into [])
                       (filter (fn [[w v]] (str/starts-with? w "z")))
                       sort reverse (map second) bin->long))

(answer-1)

;; (def answer-1 nil)
;; (def answer-2 nil)
;; (defn- -main [& _]
;;   (println "Day XX, Part 1:" answer-1)
;;   (println "Day XX, Part 2:" answer-2))

;; (-main)
