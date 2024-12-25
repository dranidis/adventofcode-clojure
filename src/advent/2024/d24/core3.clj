(ns advent.2024.d24.core3
  (:require
   [advent.util :refer [bin-vec->long long->bin-vec]]
   [clojure.string :as str]
   [clojure.test :refer [is]]
   [clojure.set :as set]))

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
                     (conj acc {:i1 i1 :i2 i2 :op g :o o}))
                   []
                   (parse-gates gates-section)))

(defn gate-value [gate v1 v2]
  (case (:op gate)
    "AND" (bit-and v1 v2)
    "OR" (bit-or v1 v2)
    "XOR" (bit-xor v1 v2)))

(defn update-wires
  [wires gates]
  (loop [wires wires
         gates gates
         pending []
         visited #{} ;; only for part 2
         cnt 0]
    (if (= cnt 10000)
      [wires visited]
      (if (empty? gates)
        (if (empty? pending)
          [wires visited]
          (recur wires pending [] visited (inc cnt)))
        (let [gate (peek gates)
              v1 (get wires (:i1 gate))
              v2 (get wires (:i2 gate))]
          (if (and (some? v1) (some? v2))
            (let [updated-wires (assoc wires (:o gate) (gate-value gate v1 v2))
                  visited (conj visited [(:op gate) (:i1 gate) (:i2 gate)])]
              (recur updated-wires (pop gates) pending visited (inc cnt)))
            (recur wires (pop gates) (conj pending (peek gates)) visited (inc cnt))))))))

(defn calc
  [wires]
  (let [[w visited] (update-wires wires gates)]
    [(->> w (into [])
          (filter (fn [[w v]] (str/starts-with? w "z")))
          sort reverse (map second)
          vec
    ;;    bin-vec->long
          )
     visited]))

(println "Day 24, Part 1" (bin-vec->long (first (calc wires))))



(defn i-name [prefix n] (->> n str
                             (#(if (= 1 (count %)) (str "0" %) %))
                             (#(str prefix %))))
(defn i-wires-names [prefix len]
  (->> (range (inc len))
       (map #(i-name prefix %))))

;; (i-wires-names "x" 5)
;; ("x00" "x01" "x02" "x03" "x04" "x05")

(defn i-values
  [n] (->> n
           (long->bin-vec)
           ((fn [v] (vec (concat (repeat (- 45 (count v)) 0) v))))
           reverse))

(defn n->wires [prefix len num] (zipmap (i-wires-names prefix len) (i-values num)))

(n->wires "x" 5 1)

(defn zero-wires [len] (merge (n->wires "x" len 0) (n->wires "y" len 0)))

;; check addition 4 cases
;; (for [b1 [1 0]
;;       b2 [1 0]
;;       n (range 6)
;;       :let [[w visited] (-> (zero-wires 45)
;;                             (assoc (i-name "x" n) b1)
;;                             (assoc (i-name "y" n) b2)
;;                             calc)
;;             bits (-> w
;;                      (#(drop (- 45 (inc n)) %))
;;                      vec)]
;;       :while (if (and (= b1 1) (= b2 1))
;;                (= (first bits) 1)
;;                (if (or (= b1 1) (= b2 1))
;;                  (= (second bits) 1)
;;                  (= (second bits) 0)))]
;;   [n [b1 b2] bits])


(let [n 5
      visited #{"x00" "y00"
                "rpj" "nsc" "y01" "gpt" "bkg" "x01"
                "y02" "x02" "mdw" "vbm" "gsh" "vgh"
                "vmd" "mkv" "vvj" "x03" "y03" "whn"
                "y04" "rkg" "tkj" "bjc" "kff" "x04"}
      t (for [b1 [0 1]
              b2 [0 1]

              :let [[w v] (-> (zero-wires n)
                              (assoc (i-name "x" n) b1)
                              (assoc (i-name "y" n) b2)
                              (#(update-wires % gates)))
                    vis-wires (set (mapcat (partial drop 1) v))]]
          [[b1 b2] (w (i-name "z" n)) (set/difference vis-wires visited) v])]
  t)


(defn update-wires-expr
  [wires gates]
  (loop [wires wires
         gates gates
         pending []
         visited #{} ;; only for part 2
         expression []
         cnt 0]
    (if (= cnt 10000)
      [wires visited expression]
      (if (empty? gates)
        (if (empty? pending)
          [wires visited]
          (recur wires pending [] visited expression (inc cnt)))
        (let [gate (peek gates)
              v1 (get wires (:i1 gate))
              v2 (get wires (:i2 gate))]
          (if (and (some? v1) (some? v2))
            (let [updated-wires (assoc wires (:o gate) (gate-value gate v1 v2))
                  visited (conj visited [(:op gate) (:i1 gate) (:i2 gate)])
                  expression (conj expression [(:op gate) (:i1 gate) (:i2 gate) (:o gate)])]
              (recur updated-wires (pop gates) pending visited expression (inc cnt)))
            (recur wires (pop gates) (conj pending (peek gates)) visited expression (inc cnt))))))))

(-> (zero-wires 1)
    (assoc (i-name "x" 1) 1)
    (assoc (i-name "y" 1) 1)
    (#(update-wires-expr % gates)))



(update-wires (zero-wires 0) gates)

(update-wires {"x00" 1 "y00" 1
               "x01" 1 "y01" 1} gates)
;; check carry
;; (for [n (range)
;;       :let [r (-> zero-wires
;;                   (assoc (i-name "x" n) 1)
;;                   (assoc (i-name "y" n) 1)
;;                   calc
;;                   (#(drop (- 45 (inc n)) %)))]
;;       :while (= (first r) 1)
;;       :while (= 0 (apply + (drop 1 r)))]
;;   n)


(defn add-bin [n1 n2]
  (calc (merge (n->wires "x" n1) (n->wires "y" n2))))

;; (last (for [x (range 100)
;;             y (range 100)
;;             :while (= (+ x y) (bin-vec->long (add-bin x y)))]
;;         [x y (bin-vec->long (add-bin x y))]))



