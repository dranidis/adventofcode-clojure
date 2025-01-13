(ns advent.2019.d9.core
  (:require
   [advent.util :refer [str->nums]]
   [clojure.string :as str]))

(def example? false)

(def example "")

(def input (if example? example (slurp "src/advent/2019/d9/input.txt")))

(def debugln str)

(defn state [instructions inputs outputs cnt base]
  {:ins (if (vector? instructions)
          (into {} (map-indexed vector instructions))
          instructions)
   :in inputs
   :out outputs
   :cnt cnt
   :base base})

(defn address [par-mode base a]
  (case par-mode
    0 a
    2 (+ base a)))

(defn values-of-args [s par-modes num]
  (let [instructions (:ins s)
        cnt (:cnt s)
        pars (map #(get instructions (+ cnt % 1)) (range num))
        pars (mapv (fn [x1 x2]
                     (if (= 1 x1)
                       x2
                       (get instructions (address x1 (:base s) x2) 0)))
                   par-modes
                   pars)]
    pars))

(defn address-for-writing [s par-mode n]
  (let [a (get (:ins s) (+ (:cnt s) n))]
    (address par-mode (:base s) a)))

(defn op2 [s par-modes par-fn]
  (let [pars (values-of-args s par-modes 2)
        at (address-for-writing s (nth par-modes 2) 3)
        res (par-fn pars)]
    (debugln "WRITE op on" pars "RES" res
             "AT" at)
    (-> s
        (assoc-in [:ins at] res)
        (update :cnt + 4))))

(defn input-op [s par-modes]
  (let [instructions (:ins s)
        outputs (:out s)
        cnt (:cnt s)
        inputs (:in s)
        at (address-for-writing s (first par-modes) 1)]
    (assert (some? at) (str "INPUT address should not be nil: " at))
    (if (empty? inputs)
      [instructions (last outputs) cnt]
      (do (debugln "INPUT " (first inputs) "AT" at)
          (-> s
              (assoc-in [:ins at] (first inputs))
              (update :in rest)
              (update :cnt + 2))))))

(defn- output-op [s par-modes]
  ;; (println (:ins s))
  (let [pars (values-of-args s par-modes 1)
        out (first pars)]
    (debugln "OUTPUT" out)
    (-> s
        (update :out #(conj % out))
        (update :cnt + 2))))

(defn- rel-op [s par-modes]
  (let [[par] (values-of-args s par-modes 1)]
    (debugln "Inc base by" par)
    (-> s
        (update :base + par)
        (update :cnt + 2))))

(defn- jump-op [s par-modes pred]
  (let [[n1 n2] (values-of-args s par-modes 2)]
    (-> s
        (update :cnt (if (pred n1) (fn [_] n2) (partial + 3))))))

(defn run-int-code-computer [instructions-i cnt inputs]
  (loop [current-state (state instructions-i inputs [] cnt 0)]
    (let [ins (get (:ins current-state) (:cnt current-state))]
      (if (= ins 99)
        (do (debugln (:out current-state))
            (last (:out current-state)))
        (let [ins-str (mapv parse-long (str/split (str ins) #""))
              [a b c d e] (vec (concat (repeat (- 5 (count ins-str)) 0) ins-str))
              par-modes [c b a]
              _ (debugln (str (mapv #(get (:ins current-state) %) (range (:cnt current-state) (+ 4 (:cnt current-state))))))
              _ (debugln "ABCDE" [a b c d e])]
          (cond
            (#{1 2 7 8} e)
            (recur (op2 current-state par-modes
                        #(case e
                           1 (apply + %)
                           2 (apply * %)
                           7 (if (apply < %) 1 0)
                           8 (if (apply = %) 1 0))))

            (= e 3)
            (let [i (input-op current-state par-modes)]
              (if (vector? i)
                i
                (recur i)))

            (= e 4)
            (recur (output-op current-state par-modes))

            (= e 5)
            (recur (jump-op current-state par-modes #(not (zero? %))))

            (= e 6)
            (recur (jump-op current-state par-modes #(zero? %)))

            (= e 9)
            (let [_ (debugln "OLD BASE" (:base current-state))
                  ns (rel-op current-state par-modes)]
              (debugln "NEW" (:base ns))
              (recur ns))


            :else (throw (ex-info (str "No ins for " e) {}))))))))

(println "ANS 1:" (run-int-code-computer
                   (str->nums input)
                   0 [1]))

(println "ANS 2:" (run-int-code-computer
                   (str->nums input)
                   0 [2]))
