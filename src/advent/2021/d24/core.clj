(ns advent.2021.d24.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]
   [criterium.core :refer [quick-bench]]))

(def example? false)

(def example "")

(def input (if example? example (slurp "src/advent/2021/d24/input.txt")))

(defn input-cmds
  [input]
  (-> input
      (str/split-lines)))

(defn read-parts
  [s cmd]
  (let [parts (str/split cmd #" ")
        part1 (get parts 1)
        part2 (get parts 2)
        a (keyword part1)
        b (if (nil? part2)
            nil
            (if-let [n (parse-long part2)]
              n
              (get s (keyword part2))))]
    [a b]))

(def cmds (input-cmds input))

(defn- process [cmds]
  ;; matching the pattern div z 1 where z can be anything
  (remove (fn [cmd] (and (str/starts-with? cmd "div")
                         (= "1" (nth (str/split cmd #" ") 2))))
          cmds))

(def cmds (process cmds))

(defn MONAD
  [digits]
  (println "Proc" digits)
  (loop [cmds cmds
         s {:x 0 :y 0 :z 0 :w 0 :d digits :r 0}
         cnt 1
         prevz nil
         prevdigit nil]
    (println s)
    (if (empty? cmds)
      (zero? (:z s))
      (let [cmd (first cmds)
            [a b] (read-parts s cmd)
            ;; _ (println cmd "A" a "B" b)
            ns (cond
                 (str/starts-with? cmd "inp")
                 (-> s
                     (assoc a (first (:d s)))
                     (update :d rest)
                     (update :r inc))

                 (str/starts-with? cmd "add")
                 (-> s
                     (update a + b))

                 (str/starts-with? cmd "mul")
                 (-> s
                     (update a * b))

                 (str/starts-with? cmd "eql")
                 (-> s
                     (update a (fn [x] (if (= x b) 1 0))))

                 (str/starts-with? cmd "div")
                 (-> s
                     (update a quot b))

                 (str/starts-with? cmd "mod")
                 (-> s
                     (update a mod b))
                 :else :error)
            z (:z ns)
            x (:x ns)
            y (:y ns)
            w (first (:d s))]
        ;; (println "CNT" (:r s) "X: " x "CMD:" cmd)

        (when (and (= cmd "inp") (= cnt 2)) ;; at beginnig of round 2 (end of 1)
          (assert (= z (inc (nth digits 0))) (str "W" w "Z" z "NS" ns))
          (assert (= y (inc (nth digits 0))))
          (assert (= x 1)))
        (when (and (= cmd "inp") (= cnt 3)) ;; at end of dig 2
          (assert  (= z (+ (* prevz 26) (nth digits 1) 9)) (str "W" w "Z" z "NS" ns))
          (assert (= y (+ w 9)))
          (assert (= x 1)))
        (recur (rest cmds) ns (inc cnt) (:z ns) w)))))


(def inputs (->> (for [i (range (parse-long (apply str (repeat 14 "9")))
                                (dec (parse-long (apply str (repeat 14 "1"))))
                                -1)]
                   (mapv parse-long (str/split (str i) #"")))
                 (remove (fn [s]
                           (some zero? s)))))


;; 99999795919428

(def inputs (->> (for [i (range 55555371514112
                                (parse-long (apply str (repeat 14 "9"))))]
                   (mapv parse-long (str/split (str i) #"")))
                 (remove (fn [s]
                           (some zero? s)))))


(set! *unchecked-math* true)

;; (def calc
;;   (memoize (fn [w z xx yy zz]
;;              (let [x (mod z 26)
;;                    z (quot z zz)
;;                    x (+ x xx)
;;                    x (if (= x w) 0 1)
;;                    y (inc (* 25 x))
;;                    z (* z y)
;;                    y (* x (+ w yy))
;;                    z (+ z y)]
;;                z))))

;; (def calc-unchecked
;;   (fn [w z xx yy zz]
;;     (let [x (unchecked-remainder-int z 26)
;;           z (unchecked-divide-int z zz)
;;           x (unchecked-add-int x xx)
;;           x (if (= x w) 0 1)
;;           y (unchecked-inc-int (unchecked-multiply-int 25 x))
;;           z (unchecked-multiply-int z y)
;;           y (unchecked-multiply-int x (unchecked-add-int w yy))
;;           z (unchecked-add-int z y)]
;;       z)))

(defn MONAD-2
  [digits]
  (loop [digits digits
  ;;   x=1    1  1  1  1      1  1  1  1      1   1   1  1
  ;;   x=0                0  -5        4   0      5 -11  6
        ;;  R    1  2  3  4  5   6  7  8  9  10 11  12  13 14
         xs [10 11 14 13 -6 -14 14 13 -8 -15 10 -11 -13 -4]
         ys  [1  9 12  6  9  15  7 12 15   3  6   2  10 12]
         zs  [1  1  1  1 26  26  1  1 26  26  1  26  26 26]
         prevz 0
         cnt 1
         prev-dig nil
         prevys nil]
    (if (empty? digits)
      (zero? prevz)
      (let [w (nth digits 0)
            xx (nth xs 0)
            yy (nth ys 0)
            zz (nth zs 0)

            x (unchecked-remainder-int prevz 26)
            z (unchecked-divide-int prevz zz)
            x (unchecked-add-int x xx)
            x (if (= x w) 0 1)
            y (unchecked-inc-int (unchecked-multiply-int 25 x))
            z (unchecked-multiply-int z y)
            y (unchecked-multiply-int x (unchecked-add-int w yy))
            z (unchecked-add-int z y)]
        ;; (when (some? prevys)
        ;;   (is (= (= x 0) (and (= zz 26) (= (- w prev-dig) (+ xx prevys))))
        ;;       (str "cnt: " cnt " x: " x " CD:" w "PD" prev-dig " xx:" xx " pys"  prevys)) ; INV
        ;;   (is (= (= x 1) (not (and (= zz 26) (= (- w prev-dig) (+ xx prevys)))))
        ;;       (str "cnt: " cnt " x: " x " CD:" w "PD" prev-dig " xx:" xx " pys" prevys)))

        ;; (is (= y (+ w yy)) (str "cnt: " cnt " y: " y))
        ;; (is (= z (+ (* prevz 26) w yy)) (str "cnt: " cnt " z: " z))

        ;; (when (= cnt 1) ;; at end of dig 1
        ;;   (assert (= z (inc w)))
        ;;   (assert (= y (inc w)))
        ;;   (assert (= x 1)))
        ;; (when (= cnt 2) ;; at end of dig 2
        ;;   (assert (and (= z (+ (* prevz 26) w 9))
        ;;                (= y (+ w 9))
        ;;                (= x 1))))
        (recur (rest digits) (rest xs) (rest ys) (rest zs) z (inc cnt)
               w yy)))))


(println (loop [inputs inputs
                cnt 0]
           (when (zero? (mod cnt 100000)) (println (first inputs)))
           (if (empty? inputs)
             nil
             (let [digits (first inputs)
                   valid? (MONAD-2 digits)]
               (if valid?
                 digits
                 (recur (rest inputs) (inc cnt)))))))

;; (MONAD [9  9  9  9  9   7  9  5  9   1  9   4   2  8])
;; (MONAD-2 [3 3 3 3 3 2 2 2 2 2 2 2 2 2])

;; (MONAD-2 [8 8 8 8 8 3 2 2 2 2 2 2 2 2]) ; 6th is 5 less than 5th => x = 0


;; in4 is ignored so put 9

;; (pop [1 2 3])
;; (quick-bench (rest [10 11 14 13 -6 -14 14 13 -8 -15 10 -11 -13 -4]))
;; (quick-bench (pop [10 11 14 13 -6 -14 14 13 -8 -15 10 -11 -13 -4]))
;; (quick-bench (first [10 11 14 13 -6 -14 14 13 -8 -15 10 -11 -13 -4]))
;; (quick-bench (nth [10 11 14 13 -6 -14 14 13 -8 -15 10 -11 -13 -4] 0))

;; ALU will run the MONAD: puzzle input
;; What is the largest model number accepted by MONAD?

;; 4th = 5th
;; 6th = 3rd-2
;; 9th = 8th+4
;; 10th = 8th - 4?
;; 12th = 11-5
;; 13th = 11th-7 (-4?)


;;          1  2  3  4  5   6  7  8  9  10 11  12  13 14
;;          9  9  9  9  9   7  9  5  9  1  9   4   5  6
(loop [ws  [5  5  3  1  1   1  9  1  5 1 6 1 1 2]
       xs [10 11 14 13 -6 -14 14 13 -8 -15 10 -11 -13 -4]
       ys  [1  9 12  6  9  15  7 12 15   3  6   2  10 12]
       zs  [1  1  1  1 26  26  1  1 26  26  1  26  26 26]
       z 0
       cnt 0]
  (println cnt (first zs) z)
  (if (empty? ws)
    nil
    (let [w (nth ws 0)
          xx (nth xs 0)
          yy (nth ys 0)
          zz (nth zs 0)

          x (unchecked-remainder-int z 26)
          z (unchecked-divide-int z zz)
          _ (when (> zz 1) (println "TEMP Z" z))
          x (unchecked-add-int x xx)
          x (if (= x w) 0 1)
          y (unchecked-inc-int (unchecked-multiply-int 25 x))
          z (unchecked-multiply-int z y)
          ;; _ (println z)
          y (unchecked-multiply-int x (unchecked-add-int w yy))
          ;; _ (println y)
          z (unchecked-add-int z y)]
      (recur (rest ws)
             (rest xs)
             (rest ys)
             (rest zs)
             z
             (inc cnt)))))

(map dec [6  6  6  6  6   4  8  1  5  0  5   0   2  3])

;; (apply str (map str [8  8  8  8  8   6  8  4  8   1  8   3   1  7])) ;; 251
(apply str (map str [9  9  9  9  9   7  9  5  9   1  9   4   2  8])) ;; 251
(apply str (map str [9 9 9 9 9 7 9 5 9 1 9 4 5 6])) ;; 251
(apply str (map str [5  5  3  1  1   1  9  1  5 1 6 1 1 2])) ;; 251

;; 77777591516134 too high
;; 88888684818317 too low ; too high 2
;; 99999795919428 too low
;; 66666481505023 toohigh
;; 55311191516112 not right

7  7  7  7  7   5  7  4  8  1  7   2   1  7

7 7 7 7 7 5 9 1 5 1 6 1 3 4

5 5 5 5 5 3 9 1 5 1 6 1 1 2