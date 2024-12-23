(ns advent.2024.d17.core
  (:require
   [advent.util :refer [str->nums]]
   [clojure.math :refer [pow]]
   [clojure.string :as str]))

(def example? false)
(def part2? true)

(def example "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(def example2 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")

(def input (if example? (if part2? example2 example) (slurp "src/advent/2024/d17/input.txt")))

;; SECTIONS
(def reg-section (first (str/split input #"\n\n")))
(def prog-section (second (str/split input #"\n\n")))

(def registers (zipmap [:A :B :C] (str->nums reg-section)))
(def program (str->nums prog-section))

(defn init-program
  [registers program]
  (assoc registers
         :pointer 0
         :program program
         :output ""))

(defn combo-operand-value
  [registers operand]
  (cond
    (<= operand 3) operand
    (= operand 4) (registers :A)
    (= operand 5) (registers :B)
    (= operand 6) (registers :C)))

(defn- update-pointer [registers]
  (let [new-pointer (+ 2 (registers :pointer))]
    (if (>= new-pointer (count (registers :program)))
      (assoc registers :pointer nil) ;; end of program
      (assoc registers :pointer new-pointer))))

(defn- dv [registers operand to]
  (assoc registers to (quot (registers :A)
                            (long (pow 2
                                       (combo-operand-value registers operand))))))

(defn- jnz [registers operand]
  (if (zero? (registers :A))
    (update-pointer registers)
    (assoc registers :pointer operand)))

(defn- out [registers operand]
  (let [result (mod (combo-operand-value registers operand) 8)
        previous-output (registers :output)]
    (assoc registers :output
           (if (= previous-output "")
             (str result)
             (str previous-output "," result)))))

;; Actual Program: 
;; 2,4, bst
;; 1,2, bix-bor
;; 7,5, cdv
;; 1,7, b-xor
;; 4,4, bxc
;; 0,3, adv
;; 5,5, out B
;; 3,0 jnz

(defn execute
  [registers op operand]
  (-> (case op
        0 (dv registers operand :A)
        1 (assoc registers :B (bit-xor (registers :B) operand))
        2 (assoc registers :B (mod (combo-operand-value registers operand) 8))
        3 (jnz registers operand)
        4 (assoc registers :B (bit-xor (registers :B) (registers :C)))
        5 (out registers operand)
        6 (dv registers operand :B)
        7 (dv registers operand :C)
        "default")
      ((fn [r] (if-not (= op 3) (update-pointer r) r)))))

(defn execute-program
  [registers program]
  (loop [registers (init-program registers program)]
    ;; (pp/pprint registers)
    (if (nil? (registers :pointer))
      (registers :output)
      (recur (execute registers
                      (nth (registers :program) (registers :pointer))
                      (nth (registers :program) (+ 1 (registers :pointer))))))))

(def answer-1 (execute-program registers program))

;; PART 2

(def int-to-3-digits-bin
  (fn [n]
    (let [s (Integer/toString n 2)]
      (str (apply str (repeat (- 3 (count s)) "0")) s))))

(def A-bin-string-iter
  (loop [A-bin-strings [{:A "" :d 1}]
         numbers []]
    (if (empty? A-bin-strings)
      numbers
      (let [A-bin-string-rec (peek A-bin-strings)
            A-bin-string (:A A-bin-string-rec)
            take-program-digits-from-the-end (:d A-bin-string-rec)]
        (if (= (inc (count program)) take-program-digits-from-the-end)
          (recur (pop A-bin-strings) (conj numbers A-bin-string))
          (let [results-3 (->> (map (fn [i]
                                      (let [s (str A-bin-string
                                                   (int-to-3-digits-bin i))]
                                        (Long/parseLong s 2)))
                                    (range 8))
                               (map #(execute-program {:A % :B 0 :C 0} program))
                               (map (fn [s] (read-string (str "[" s "]")))))
                last-program (drop (- (count program) take-program-digits-from-the-end) program)
                results-filtered (filter (fn [[i v]]
                                           (= v last-program))
                                         (map-indexed vector
                                                      results-3))]
            (if (empty? results-filtered)
              (recur (pop A-bin-strings) numbers)
              (let [high-bits-0-list
                    (map (fn [r] (int-to-3-digits-bin (first r))) results-filtered)
                    bit-strings (map (fn [h] {:A (str A-bin-string h)
                                              :d (inc take-program-digits-from-the-end)})
                                     high-bits-0-list)]
                (recur (apply conj (pop A-bin-strings) bit-strings) numbers)))))))))

(def answer-2 (apply min (map (fn [s] (Long/parseLong s 2)) A-bin-string-iter)))

(defn- -main [& _]
  (println "Day 17, Part 1:" answer-1)
  (println "Day 17, Part 2:" answer-2))