(ns advent.2021.d24.core
  (:require
   [clojure.string :as str]))

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

(def cmds (input-cmds (slurp "src/advent/2021/d24/input.txt")))

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

(comment
  (def inputs (->> (for [i (range (parse-long (apply str (repeat 14 "9")))
                                  (dec (parse-long (apply str (repeat 14 "1"))))
                                  -1)]
                     (mapv parse-long (str/split (str i) #"")))
                   (remove (fn [s]
                             (some zero? s)))))

;; Takes for ever
  (println (loop [inputs inputs
                  cnt 0]
             (when (zero? (mod cnt 100000)) (println (first inputs)))
             (if (empty? inputs)
               nil
               (let [digits (first inputs)
                     valid? (MONAD digits)]
                 (if valid?
                   digits
                   (recur (rest inputs) (inc cnt))))))))

;; After analyzing MY input file
;; this is what is being executed in the 14 rounds
;; Other input files use different constants but PROBABLY same logic
(defn monad-analyzed [ws]
  (loop [ws  ws
         xs  [10 11 14 13  -6  -14      14 13 -8 -15 10 -11 -13 -4]
         ys  [1  9 12  6  9  15  7 12 15   3  6   2  10 12]
         zs  [1  1  1  1 26  26  1  1 26  26  1  26  26 26]
         z 0
         X 0
         cnt 0]
    (if (empty? ws)
      X
      (let [w (nth ws 0)
            xx (nth xs 0)
            yy (nth ys 0)
            zz (nth zs 0)

            x (mod z 26)
            z (quot z zz) ;; gets smaller in some rounds
            x (+ x xx)
            x (if (= x w) 0 1)
            y (inc (* 25 x))
            z (* z y)
            y (* x (+ w yy))
            z (+ z y)]

        (recur (rest ws)
               (rest xs)
               (rest ys)
               (rest zs)
               z
               x
               (inc cnt))))))

;; To optimize the loops:
;; 4th = 5th
;; 6th = 3rd-2
;; 9th = 8th+4
;; 12th = 11-5
(println "ANS 1"
         (->> (first
               (for [x1 (range 9 0 -1)
                     x2 (range 9 0 -1)
                     x3 (range 9 4 -1)
                     x45 [9]
                     x7 (range 9 0 -1)
                     x8 (range 5 0 -1)
                     :let [x-r9 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4)])]
                     :when (zero? x-r9)
                     x10 (range 9 0 -1)
                     :let [x-r10 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10])]
                     :when (zero? x-r10)
                     x11 (range 9 5 -1)
                     :let [x-r12 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5)])]
                     :when (zero? x-r12)
                     x13 (range 9 4 -1)
                     :let [x-r13 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5) x13])]
                     :when (zero? x-r13)
                     x14 (range 9 4 -1)
                     :let [x-r14 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5) x13 x14])]
                     :when (zero? x-r14)]
                 [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5) x13 x14]))
              (map str)
              (apply str)))

(println "ANS 2"
         (->> (first
               (for [x1 (range 1 10)
                     x2 (range 1 10)
                     x3 (range 3 10)
                     x45 [1]
                     x7 (range 1 10)
                     x8 (range 1 6)
                     :let [x-r9 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4)])]
                     :when (zero? x-r9)
                     x10 (range 1 10)
                     :let [x-r10 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10])]
                     :when (zero? x-r10)
                     x11 (range 6 10)
                     :let [x-r12 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5)])]
                     :when (zero? x-r12)
                     x13 (range 1 10)
                     :let [x-r13 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5) x13])]
                     :when (zero? x-r13)
                     x14 (range 1 10)
                     :let [x-r14 (monad-analyzed [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5) x13 x14])]
                     :when (zero? x-r14)]
                 [x1 x2 x3 x45 x45 (- x3 2) x7 x8 (+ x8 4) x10 x11 (- x11 5) x13 x14]))
              (map str)
              (apply str)))
