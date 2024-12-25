(ns advent.2024.d24.core
  (:require
   [advent.util :refer [bin->long in-vector?]]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

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

(defn all-wires
  [wires gates]
  (reduce (fn [w gate]
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

;; (def bits (map second (reverse (sort (filter (fn [[w v]] (str/starts-with? w "z"))
;;                                              (into [] (settled  gates 100 all-wires)))))))

(defn answer-1 [] (->> (all-wires wires gates)
                       (settled gates 10000000)
                       (into [])
                       (filter (fn [[w v]] (str/starts-with? w "z")))
                       sort reverse (map second) bin->long))


(answer-1)

(def w {"x00" 1, "y00" 1})
(->> (all-wires wires gates)
     (settled gates 100000)
     (into [])
     (filter (fn [[w v]] (str/starts-with? w "z")))
     sort reverse
     (map second))


;; part 2

(comment
  ;; 45 bits

  ;; 44 OR
  ;; 89 XOR
  ;; 89 AND

  ;; 1 Half (XOR, AND) and 44 Full Adders (2XOR, 2 AND, 1 OR)

  ;; (+ 44 89 89)
  ;
  )

(def get-o (fn [g] (get g :o)))
(def get-is (fn [g] [(get g :i1) (get g :i2)]))

(defn with-op [op]
  (fn [gates]
    (->> gates
         (filter #(= (:op %) op)))))

(defn with-input [inp]
  (fn [gates]
    (->> gates
         (filter (fn [g] (or (= inp (get g :i1))
                             (= inp (get g :i2))))))))

(defn with-output [o]
  (fn [gates]
    (->> gates
         (filter (fn [g] (= o (get g :o)))))))

(defn with-output-matching [m]
  (fn [gates]
    (->> gates
         (filter (fn [g] (str/starts-with? (:o g) m))))))

(defn with-input-matching [m]
  (fn [gates]
    (->> gates
         (filter (fn [g] (or (str/starts-with? (:i1 g) m)
                             (str/starts-with? (:i2 g) m)))))))

(defn pgate [g] [(:i1 g) (:i2 g) (:op g) (:o g)])

(defn in-inputs [gate i]
  (#{(get gate :i1) (get gate :i2)} i))

(defn check-full-adder
  [gates c00 cnt]
  (println "CHECK FULL ADDER")

  (let [cnt (inc cnt)
        cnt-str (str cnt)
        cnt-str (if (= 1 (count cnt-str)) (str "0" cnt-str) cnt-str)
        zXX (str "z" cnt-str)
        xXX (str "x" cnt-str)

        _ (println cnt-str)
        cc01 (->> gates ((with-input c00)))
        _ (println "Gates with input " c00 ":" cc01)
        xorc01 (first (->> cc01 ((with-op "XOR"))))
        _ (println "Checking carry XOR:" xorc01)]
        ;; _ (is (= zXX (:o xorc01)) (str (:o xorc01) " NOT " zXX))
    (if (not= zXX (:o xorc01))
      [zXX (:o xorc01)]
      (let [andc01 (first (->> cc01 ((with-op "AND"))))
            _ (println andc01)
            xorc01_i (first (disj #{(:i1 xorc01) (:i2 xorc01)} c00))
            i01 (first (->> gates ((with-output xorc01_i))))
            _ (println i01)
            _ (is (in-inputs i01 xXX))
            _ (is (= "XOR" (:op i01)))
            orc01 (first (->> gates ((with-input (:o andc01)))))
            _ (println orc01)
            orc01_i (first (disj #{(:i1 orc01) (:i2 orc01)} (:o andc01)))
            and01 (first (->> gates ((with-output orc01_i))))
            _ (println and01)
            _ (is (in-inputs and01 xXX))
            _ (is (= "AND" (:op and01)))
            c01 (:o orc01)]
        c01))))

(defn- swap
  [gates [f s]]
  (println "SWAP pair of wires" [f s])
  (reduce (fn [acc g]
            (let [g (if (= s (:o g))
                      (assoc g :o f)
                      (if (= f (:o g))
                        (assoc g :o s)
                        g))
                  g (if (= s (:i1 g))
                      (assoc g :i1 f)
                      (if (= f (:i1 g))
                        (assoc g :i1 s)
                        g))
                  g (if (= s (:i2 g))
                      (assoc g :i2 f)
                      (if (= f (:i2 g))
                        (assoc g :i2 s)
                        g))]
              (conj acc g)))
          []
          gates))

(->> gates ((with-input "jst")) (map pgate))
(->> (swap gates ["jst" "z05"]) ((with-input "jst")) (map pgate))


(defn check-and-fix-circuit
  [gates]
  (let [i00 (->> gates ((with-input "x00")))
        xor00 (first (->> i00 ((with-op "XOR"))))
        and00 (first (->> i00 ((with-op "AND"))))
        _ (is (= "z00" (:o xor00)))
        _ (is (= "x00" (:i1 xor00)))

        c00 (:o and00)]
    (loop [gates gates
           carry c00
           wrong-wires []
           cnt 0
           timeout 0]
      (if (= timeout 1000)
        :cannot-repair
        (if (= cnt 45)
          gates
          (let [carry (check-full-adder gates carry cnt)]
            (if (vector? carry)
              (let [gates (swap gates carry)
                    wrong-wires (apply conj wrong-wires carry)]
                (recur gates c00 wrong-wires 0 (inc timeout)))
              (recur gates carry wrong-wires (inc cnt) (inc timeout)))))))))

(check-and-fix-circuit gates)

;; 

;; ALL ANDs, except one, should be connected to an OR 
;; (inside a Full adder, 88 ANDs in 44 Full Adders).
;; The last AND (of the Half adder) should be connected 
;; to a XOR of the next Full Adder.
;; 2 ANDs are connected to XOR gates (expected one)
(def and-gates-that-are-connected-to-XOR-expect-z01
  (->> gates
       ((with-op "AND"))
       (filter #(in-vector?
                 (vec (->> gates
                           ((with-op "XOR"))
                           (remove (fn [g] (= "z01" (:o g))))
                           (map get-is)
                           (flatten)))
                 (get % :o)))
       (map pgate)))
;; ["x10" "y10" "AND" "mcm"] 

(def wrong-wires
  (let [non-xor-to-z-except-last-carry
        (->> gates
             ((with-output-matching "z"))
             (filter #(not= (:op %) "XOR"))
             (remove (fn [g]  (= "z45" (get g :o)))) ;; last full adder is correct
             (map get-o))

        and-gates-that-are-connected-to-XOR-expect-z01
        (->> gates ((with-op "AND"))
             (filter #(in-vector?
                       (vec (->> gates ((with-op "XOR"))
                                 (remove (fn [g] (= "z01" (:o g))))
                                 (map get-is)
                                 (flatten)))
                       (get % :o)))
             (map get-o))

        inputs_of_OR_gates_that_are_not_from_ANDs
        (flatten (for [org (->> gates ((with-op "OR")))]
                   (->> gates
                        ((with-output (:i1 org)))
                        (remove (fn [g] (= "AND" (get g :op))))
                        (map get-o))))]
    (concat non-xor-to-z-except-last-carry
            and-gates-that-are-connected-to-XOR-expect-z01
            inputs_of_OR_gates_that_are_not_from_ANDs)))

(def answer-2 (str/join "," (sort ["z30" "z05" "z15" "mcm" "gdf" "gwc" "dnt" "jst"])))


(->> gates ((with-op "AND"))
     (filter #(in-vector?
               (vec (->> gates ((with-op "XOR"))
                        ;;  (remove (fn [g] (= "z01" (:o g))))
                         (map get-is)
                         (flatten)))
               (get % :o)))
     (map get-o))

(->> gates ((with-output "vmd")) (map pgate))
(->> gates ((with-input "mkv")) (map pgate))
;; (pp/pp)
;; HALF ADDER

;; ["y01" "x01" "XOR" "nsc"] ["y01" "x01" "AND" "bkg"]

(->> gates ((with-input-matching "x00"))
     ((with-op "XOR"))
     (map pgate))
;; ["y01" "x01" "XOR" "nsc"]  ;; Shouldn't this be to z01?
(->> gates ((with-input-matching "x00"))
     ((with-op "AND"))
     (map pgate))
;; ["y01" "x01" "AND" "bkg"]


;; AND and XORs for addition
(for [w (->> (all-wires wires gates)
             (into [])
             (filter (fn [[w -]] (str/starts-with? w "x")))
             (map first)
             sort)]
  (->> gates ((with-input w)) (map pgate)))

;; 
;; XOR for outpus (plus one OR for the last)
;; 
(for [w (->> (all-wires wires gates)
             (into [])
             (filter (fn [[w -]] (str/starts-with? w "z")))
             (map first)
             sort)]
  (->> gates ((with-output w)) (map pgate)))
;; WRONG:
;; ["sgt" "bhb" "OR" "z05"]
;; ["y15" "x15" "AND" "z15"]
;; ["kgr" "vrg" "AND" "z30"]

;; 
;; All ORs should receive inputs from ANDs
;; 
(for [org (->> gates ((with-op "OR")))]
  (->> gates ((with-output (:i1 org))) (map pgate)))
;; WRONG
;; ["y10" "x10" "XOR" "gdf"]
;; ["kgr" "vrg" "XOR" "gwc"]
;; ["vhr" "dvj" "XOR" "dnt"]

(for [org (->> gates ((with-op "OR")))]
  (->> gates ((with-output (:i2 org))) (map pgate)))

;; ALL Internal gates correctly go to OR gates
;; except the one going to the XOR/AND for z01
(->>
 (->> gates ((with-op "AND"))
      (remove (fn [g] (str/starts-with? (:i1 g) "x"))))
 (mapcat (fn [internal-agate]
           (->> gates ((with-input (get internal-agate :o)))
                (remove (fn [g] (= "OR" (:op g))))
                (map pgate)))))


;; INTERNAL XOR gates should only go to XOR and AND
(remove (fn [internal-xor-out]
          (empty? (->> gates
                       ((with-input internal-xor-out))
                       (remove (fn [g] (or (= "XOR" (:op g))
                                           (= "AND" (:op g)))))
                       (map pgate))))
        (->> gates ((with-op "XOR"))
             (remove (fn [g] (str/starts-with? (:o g) "z")))
             (map get-o)))
;; ("gwc" "dnt" "gdf") NOTHING NEW already! Go to OR!




;; (def answer-1 nil)
;; (def answer-2 nil)
;; (defn- -main [& _]
;;   (println "Day XX, Part 1:" answer-1)
;;   (println "Day XX, Part 2:" answer-2))

;; (-main)


