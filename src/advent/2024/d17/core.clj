(ns advent.2024.d17.core
  (:require
   [advent.util :refer [str->nums]]
   [clojure.math :refer [pow]]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(def example? false)

(def example "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(def input (if example? example (slurp "src/advent/2024/d17/input.txt")))

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

;; (init-program registers program)

;; (quot 117440 8)

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

(defn- adv [registers operand]
  (let [numerator (registers :A)
        denominator (long (pow 2 (combo-operand-value registers operand)))
        result (quot numerator denominator)]
    (-> registers
        (assoc :A result)
        (update-pointer))))

(defn- bdv [registers operand]
  (let [numerator (registers :A)
        denominator (long (pow 2 (combo-operand-value registers operand)))
        result (quot numerator denominator)]
    (-> registers
        (assoc :B result)
        (update-pointer))))

(defn- cdv [registers operand]
  (let [numerator (registers :A)
        denominator (long (pow 2 (combo-operand-value registers operand)))
        result (quot numerator denominator)]
    (-> registers
        (assoc :C result)
        (update-pointer))))

(defn- bit-XOR [registers operand]
  (let [b (registers :B)
        result (bit-xor b operand)]
    (update-pointer (assoc registers :B result))))

(defn- bst [registers operand]
  (update-pointer
   (assoc registers :B (mod (combo-operand-value registers operand) 8))))

(defn- jnz [registers operand]
  (if (zero? (registers :A))
    (update-pointer registers)
    (assoc registers :pointer operand)))

(defn- bxc [registers _]
  (let [b (registers :B)
        c (registers :C)
        result (bit-xor b c)]
    (update-pointer (assoc registers :B result))))

(defn- out [registers operand]
  (let [result (mod (combo-operand-value registers operand) 8)
        previous-output (registers :output)]
    (update-pointer (assoc registers :output
                           (if (= previous-output "")
                             (str result)
                             (str previous-output "," result))))))



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
  (case op
    0 (adv registers operand)
    1 (bit-XOR registers operand)
    2 (bst registers operand)
    3 (jnz registers operand)
    4 (bxc registers operand)
    5 (out registers operand)
    6 (bdv registers operand)
    7 (cdv registers operand)
    "default"))

(defn execute-program
  [registers program]
  (loop [registers (init-program registers program)]
    ;; (pp/pprint registers)
    (if (nil? (registers :pointer))
      (registers :output)
      (recur (execute registers
                      (nth (registers :program) (registers :pointer))
                      (nth (registers :program) (+ 1 (registers :pointer))))))))

(defn execute-program-interrupt
  [registers program]
  (loop [registers-all (init-program registers program)]
    ;; (pp/pprint registers-all)
    (let [out (vec (remove nil?
                           (mapv parse-long (str/split (registers-all :output) #","))))]
      ;; (println "out" out)
      (if (or (nil? (registers-all :pointer))
              (not= out (vec (take (count out) program))))
        (registers-all :output)
        (recur (execute registers-all
                        (nth (registers-all :program) (registers-all :pointer))
                        (nth (registers-all :program) (+ 1 (registers-all :pointer)))))))))

(def answer-1 (execute-program registers program))

(defn mutate-program
  [registers program]
  (loop [n 8]
    (when (zero? (mod n 1000))
      (println n))
    (let [registers (assoc registers :A n)
          out (execute-program-interrupt registers program)]
      ;; (println n out)
      (if (= program
             (mapv parse-long (str/split out #",")))
        n
        (recur (* 2 n))))))

(def answer-2 (mutate-program {:A 2024 :B 0 :C 0} [0,3,5,4,3,0])) ;; WORKS!!! 117440
;; (def ex (execute-program {:A 117440 :B 0 :C 0} [0,3,5,4,3,0])) ;; WORKS!!! 117440


;; ACTUAL PROGRAM ASNWER: TOO LONG
;; (def answer-2 (mutate-program registers program))

;; (def answer-2 nil)
(defn- -main [& _]
  (println "Day XX, Part 1:" answer-1)
  (println "Day XX, Part 2:" answer-2))

(-main)

