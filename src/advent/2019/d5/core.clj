(ns advent.2019.d5.core
  (:require
   [advent.2019.d9.core :refer [run-int-code-computer-all visited]]
   [advent.util :refer [str->nums]]))

(def instructions (-> (slurp "src/advent/2019/d5/input.txt")
                      str->nums))
;; (def debugln str)

;; (defn run-int-code-computer [instructions cnt inputs]
;;   (loop [instructions instructions
;;          inputs inputs
;;          outputs []
;;          cnt cnt]
;;     (let [ins (nth instructions cnt)]
;;       ;; (debugln "CNT" cnt "INS"  ins)
;;       (if (= ins 99)
;;         (last outputs)
;;         (let [s (mapv parse-long (str/split (str ins) #""))
;;               [a b c d e] (vec (concat (repeat (- 5 (count s)) 0) s))
;;               ;; _ (debugln "ABCDE" [a b c d e])
;;               [n1 n2] (map #(nth instructions (+ cnt %)) [1 2])
;;               n1 (if (zero? c) (get instructions n1) n1)
;;               n2 (if (zero? b) (get instructions n2) n2)]
;;           (cond
;;             (#{1 2} e)
;;             (let [at (nth instructions (+ cnt 3))
;;                   res (case e 1 (+ n1 n2) (* n1 n2))
;;                   ;; _ (debugln "WRITE " res "AT" at)
;;                   instructions (assoc instructions at res)]
;;               (recur instructions inputs outputs (+ 4 cnt)))

;;             (= e 3)
;;             (let [at (nth instructions (+ cnt 1))]
;;               (if (empty? inputs)
;;                 [instructions (last outputs) cnt]
;;                 (do (debugln "INPUT " (first inputs) "AT" at)
;;                     (recur (assoc instructions at (first inputs))
;;                            (rest inputs) outputs (+ cnt 2)))))

;;             (= e 4)
;;             (let [out n1]
;;               (debugln "OUTPUT" out)
;;               (recur instructions inputs (conj outputs out) (+ cnt 2)))

;;             (= e 5)
;;             (do
;;               ;; (debugln "JUMP IF NON ZERO " n1 n2)
;;               (if-not (zero? n1)
;;                 (recur instructions inputs outputs n2)
;;                 (recur instructions inputs outputs (+ cnt 3))))

;;             (= e 6)
;;             (do
;;               ;; (debugln "JUMP IF ZERO" n1 n2)
;;               (if (zero? n1)
;;                 (recur instructions inputs outputs n2)
;;                 (recur instructions inputs outputs (+ cnt 3))))

;;             (= e 7)
;;             (let [at (nth instructions (+ cnt 3))
;;                   instructions (assoc instructions at (if (< n1 n2) 1 0))]
;;               (recur instructions inputs outputs (+ cnt 4)))

;;             (= e 8)
;;             (let [at (nth instructions (+ cnt 3))
;;                   res (if (= n1 n2) 1 0)
;;                   ;; _ (debugln "IF eq" n1 n2 "WRITE " res "AT" at)
;;                   instructions (assoc instructions at res)]
;;               (recur instructions inputs outputs (+ cnt 4)))

;;             :else (throw (ex-info (str "No ins for " e) {}))))))))


(defn- -main [& _]
  (println "ANS 1:" (last (run-int-code-computer-all instructions 0 0 [1])))
  (println "ANS 2:" (last (run-int-code-computer-all instructions 0 0 [5])))
  (println (->> @visited (into []) sort)))

