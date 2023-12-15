(ns advent.2023.d15.core
  (:require [clojure.string :as str]))

(def input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn hash-algorithm [s]
  (reduce (fn [current x] (-> current (+ x) (* 17) (mod 256)))
          0
          (map int s)))

(defn answer-1-sum-alg [input]
  (apply + (map hash-algorithm
                (str/split input #","))))

;;  --- Part Two ---

(defn parse [input]
  (for [step (str/split input #",")]
    (let [[[_ label op focal]] (re-seq #"(.*)([=\-])(\d*)" step)
          focal (if (= focal "") nil (parse-long focal))]
      [label op focal])))

(defn step [boxes [label op focal]]
  (let [the-box-id (hash-algorithm label)
        indexed-lenses (vec (map-indexed vector (get boxes the-box-id)))
        has-label? (fn [[_ [lbl _]]] (= lbl label))]
    (if (= op "-")
      (assoc boxes the-box-id
             (mapv second (remove has-label? indexed-lenses)))

      (if-let [[index _] (first (filter has-label? indexed-lenses))]
        (update boxes the-box-id (fn [box]
                                   (assoc box index [label focal])))
        (update boxes the-box-id conj [label focal])))))

(defn answer-2 [input]
  (let [boxes-init (zipmap (range 256)
                           (map (fn [_] []) (range 256)))
        boxes-after-steps (reduce step
                                  boxes-init
                                  (parse input))]
    (apply + (flatten (map (fn [[box-nr lenses-list]]
                             (map (fn [[pos-in-box [_ focal]]]
                                    (* (inc box-nr) (inc pos-in-box) focal))
                                  (map-indexed vector lenses-list)))
                           boxes-after-steps)))))

(defn -main [& _]
  (let [input (slurp "src/advent/2023/d15/input.txt")]
    (time (println "Day 15, part 1:"
                   (answer-1-sum-alg input)))
    (time (println "Day 15, part 2:"
                   (answer-2 input)))))
