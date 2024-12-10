(ns advent.2021.d16.core)

(def hex-input (slurp "src/advent/2021/d16/input.txt"))

(defn hex-to-binary [hex]
  (let [hex-map {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
                 \4 "0100" \5 "0101" \6 "0110" \7 "0111"
                 \8 "1000" \9 "1001" \A "1010" \B "1011"
                 \C "1100" \D "1101" \E "1110" \F "1111"}]
    (apply str (map hex-map hex))))

(defn parse-binary [binary n]
  [(Integer/parseInt (apply str (take n binary)) 2) (drop n binary)])

(defn parse-literal [binary]
  (loop [remaining binary
         value ""]
    (let [[prefix chunk rest] [(first remaining)
                               (take 4 (drop 1 remaining))
                               (drop 5 remaining)]]
      (if (= prefix \0)
        [(Long/parseLong (str value (apply str chunk)) 2) rest]
        (recur rest (str value (apply str chunk)))))))

(declare parse-packet)

(defn parse-packets [binary]
  (loop [remaining binary
         packets []]
    (if (empty? remaining)
      packets
      (let [[packet rest] (parse-packet remaining)]
        (recur rest (conj packets packet))))))

(defn parse-operator [binary length-type]
  (if (= length-type \0)
    (let [[total-length binary] (parse-binary binary 15)
          [sub-packets rest] (split-at total-length binary)]
      [(parse-packets sub-packets) rest])
    (let [[num-sub-packets binary] (parse-binary binary 11)]
      (loop [remaining binary
             packets []
             count num-sub-packets]
        (if (zero? count)
          [packets remaining]
          (let [[packet rest] (parse-packet remaining)]
            (recur rest (conj packets packet) (dec count))))))))

(defn parse-packet [binary]
  (let [[version binary] (parse-binary binary 3)
        [type-id binary] (parse-binary binary 3)]
    (if (= type-id 4)
      (let [[value rest] (parse-literal binary)]
        [{:version version :type :literal :value value} rest])
      (let [[length-type-id binary] [(first binary) (rest binary)]
            [sub-packets rest] (parse-operator binary length-type-id)
            operator (case type-id
                       0 :sum
                       1 :product
                       2 :minimum
                       3 :maximum
                       5 :greater-than
                       6 :less-than
                       7 :equal)]
        [{:version version :type operator :sub-packets sub-packets} rest]))))

(defn sum-versions [packet]
  (if (= (:type packet) :literal)
    (:version packet)
    (+ (:version packet) (reduce + (map sum-versions (:sub-packets packet))))))

(defn solve [hex-input]
  (let [binary (hex-to-binary hex-input)]
    (-> binary
        (parse-packet)
        first
        (sum-versions))))

(def answer-1 (solve hex-input))

(defn evaluate [packet]
  (if (= (:type packet) :literal)
    (:value packet)
    (case (:type packet)
      :sum (reduce + (map evaluate (:sub-packets packet)))
      :product (reduce * (map evaluate (:sub-packets packet)))
      :minimum (apply min (map evaluate (:sub-packets packet)))
      :maximum (apply max (map evaluate (:sub-packets packet)))
      :greater-than (if (> (first (map evaluate (:sub-packets packet)))
                           (second (map evaluate (:sub-packets packet))))
                      1 0)
      :less-than (if (< (first (map evaluate (:sub-packets packet)))
                        (second (map evaluate (:sub-packets packet))))
                   1 0)
      :equal (if (= (first (map evaluate (:sub-packets packet)))
                    (second (map evaluate (:sub-packets packet))))
               1 0))))

(defn solve2 [hex-input]
  (let [binary (hex-to-binary hex-input)]
    (-> binary
        (parse-packet)
        first
        (evaluate))))

(def answer-2 (solve2 hex-input))

(defn -main [& _]
  (println "Day 16, Part 1:" answer-1)
  (println "Day 16, Part 2:" answer-2))