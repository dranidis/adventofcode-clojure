(ns advent.2020.d23.core
  (:require
   [advent.util :refer [in-list? in-vector?]]))

(def example? false)
(def example "389125467")
(def input (if example? example "219748365"))

(def cups (->> input vec (mapv (comp parse-long str))))

(defn cups-loop [times max-n cups]
  (loop [cups (cycle cups)
         cnt 0]
    (if (= cnt times)
      cups
      (let [c (first cups)
            pick-up (->> cups (drop 1) (take 3))
            destination (loop [d (dec c)]
                          (if (zero? d)
                            (recur max-n)
                            (if (in-list? pick-up d)
                              (recur (dec d))
                              d)))
            ;; _ (println "DEST" destination)
            before-destination (loop [c (->> cups (drop 4))
                                      b []]
                                 (if (= destination (first c))
                                   b
                                   (recur (->> c (drop 1)) (conj b (first c)))))
            ;; _ (println "BEF DEST" before-destination)
            after-destination (->> cups (drop (+ 5 (count before-destination))))
            ;; _ (println "AFTER" (take 10 after-destination))
            cups (->> (cycle (take max-n (concat
                                          before-destination
                                          [destination]
                                          pick-up
                                          after-destination))))]
        (recur cups (inc cnt))))))

(time (->> cups
           (cups-loop 100 (apply max cups))
           ((fn [cups] (loop [c cups]
                         (if (= 1 (first c))
                           (->> c (remove #(= % 1)) (take 8))
                           (recur (->> c (drop 1)))))))
           (apply str)
           (println "ANS 1: ")))

;; PART 2

;; Using and updating a dictionary (after) to store the next node of a node
;; instead of storing the array of nodes
(defn loop-after [times max-n cups]
  (loop [after (->> cups (#(conj % (first cups))) (partition 2 1) (map vec) (into {}))
         current (first cups)
         cnt 0]
    (if (= cnt times)
      after
      (let [pick-up [(-> current after)
                     (-> current after after)
                     (-> current after after after)]
            last-pickup (last pick-up)
            destination (loop [d (dec current)]
                          (if (zero? d)
                            (recur max-n)
                            (if (in-vector? pick-up d)
                              (recur (dec d))
                              d)))
            after (-> after
                      (assoc current (after last-pickup))
                      (assoc last-pickup (after destination))
                      (assoc destination (first pick-up)))
            current (after current)]
        (recur after current (inc cnt))))))

(time (->> cups
           (loop-after 100 (apply max cups))
           ((fn [after] (loop [d 1
                               dig []]
                          (let [after-d (after d)]
                            (if (= 1 after-d)
                              dig
                              (recur after-d (conj dig after-d)))))))
           (apply str)
           (println "ANS 1 (using after dict): ")))

(time (let [cups (vec (concat cups (range 10 1000001)))]
        (->> cups
             (loop-after 10000000 (apply max cups))
             ((fn [after] [(-> 1 after) (-> 1 after after)]))
             (apply *)
             (println "ANS 2: "))))


