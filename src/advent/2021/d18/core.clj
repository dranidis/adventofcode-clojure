(ns advent.2021.d18.core
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.test :refer [is]] ;;  [clojure.walk :refer [postwalk prewalk]]
   ))

(def example? false)

(def input (if example? "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
               (slurp "src/advent/2021/d18/input.txt")))

(defn parse
  [input]
  (mapv read-string (str/split-lines input)))


;; (defn- parse [input]
;;   (mapv parse-long (str/split input #"")))



;; (defn parse [input]
;;   (for [line (str/split-lines input)]
;;     (mapv parse-long (re-seq #"\d+" line))))



;; (def grid (str->2D input))
;; (def rows (count grid))
;; (def cols (count (first grid)))


;; (defn- -main [& _]
;;   (println "Day 9, Part 1:" answer-1)
;;   (println "Day 9, Part 2:" answer-2))

(defn make-snailfish
  [a b]
  ;; (println "CONS" a b)
  [a b])

(defn s-vector?
  [snailfish]
  (vector? snailfish))

(defn s-first
  [snailfish]
  ;;  (prn "FIRST" snailfish)
  (first snailfish))

(defn s-second
  [snailfish]
  (second snailfish))

(defn nested?
  [snailfish]
  (letfn [(count-levels [snailfish levels]
            (if (s-vector? snailfish)
              (let [left-level (count-levels (s-first snailfish) (inc levels))
                    right-level (count-levels (s-second snailfish) (inc levels))]
                (max left-level right-level))
              levels))]
    (> (count-levels snailfish 0) 4)))

(nested? [[[[[9,8],1],2],3],4])
(nested? [[[[0,9],2],3],4])

(defn add-to-first-left-number
  [num value]
  ;; (println "ADD L" num value)
  (if (s-vector? num)
    (make-snailfish (add-to-first-left-number (s-first num) value) (s-second num))
    (+ num value)))

(defn add-to-first-right-number
  [num value]
  ;; (println "ADD R" num value)
  (if (s-vector? num)
    (make-snailfish (s-first num) (add-to-first-right-number (s-second num) value))
    (+ num value)))

(defn s-explode
  [snailfish]
  (letfn [(reduce-nested-level [snailfish levels]
            ;; (prn snailfish levels)
            (if (s-vector? snailfish)
              (if (>= levels 4)
                [0 snailfish false false]
                (let [;; _ (println "Going L")
                      [reduced-snailfish old-pair added-first? added-second?]
                      (reduce-nested-level (s-first snailfish) (inc levels))]
                  ;; (println "REDUCED 1st" reduced-snailfish "OLD" old-pair "L" levels "was" snailfish)
                  (if old-pair
                    (if (and (= levels 3) (not (s-vector? (s-second snailfish))))
                      [(make-snailfish reduced-snailfish (+ (s-second old-pair)
                                                            (s-second snailfish)))
                       old-pair added-first? true]
                      (if added-second?
                        [(make-snailfish reduced-snailfish (s-second snailfish)) old-pair added-first? added-second?]
                        [(make-snailfish reduced-snailfish (add-to-first-left-number
                                                            (s-second snailfish)
                                                            (s-second old-pair))) old-pair added-first? true]))
                    (let [;; _ (println "Going R")
                          [reduced-snailfish old-pair added-first? added-second?]
                          (reduce-nested-level (s-second snailfish) (inc levels))]
                      ;; (println "REDUCED 2nd" reduced-snailfish "OLD" old-pair "L" levels "was" snailfish)
                      (if old-pair
                        (if added-first?
                          [(make-snailfish (s-first snailfish) reduced-snailfish) old-pair added-first? added-second?]
                          [(make-snailfish (add-to-first-right-number
                                            (s-first snailfish)
                                            (s-first old-pair)) reduced-snailfish) old-pair true added-second?])
                        [snailfish nil added-first? added-second?])))))
              ;; number 
              [snailfish nil false false]))]
    ;; (println "EXPLODE" snailfish)
    (first (reduce-nested-level snailfish 0))))

(is (= [[[[0,9],2],3],4] (s-explode [[[[[9,8],1],2],3],4])))
(is (= [7,[6,[5,[7,0]]]] (s-explode [7,[6,[5,[4,[3,2]]]]])))
(is (= [[6,[5,[7,0]]],3] (s-explode [[6,[5,[4,[3,2]]]],1])))
(is (= [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (s-explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])))
(is (= [[3 [3 [0 9]]] [6 [5 [4 [3 2]]]]]
       (s-explode [[3,[2,[[1 2],7]]],[6,[5,[4,[3,2]]]]])))
(is (= [[3,[2,[8,0]]],[9,[5,[7,0]]]] (s-explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])))




(defn number-greater-than-ten?
  [snailfish]
  (if (s-vector? snailfish)
    (or (number-greater-than-ten? (s-first snailfish))
        (number-greater-than-ten? (s-second snailfish)))
    (>= snailfish 10)))

(s-vector? 0)
(def snailfish 0)
(defn split-snailfish
  [snailfish]
  ;; (prn "SPLIT" snailfish)
  (letfn  [(spli [snailfish]
            ;;  (prn "SPLI" snailfish)
             (if (s-vector? snailfish)
               (let [;;  _ (prn "SPLITTING" snailfish)
                     [splitted old-number] (spli (s-first snailfish))]
                 (if old-number
                   [(make-snailfish splitted (s-second snailfish)) old-number]
                   (let [[splitted old-number] (spli (s-second snailfish))]
                     (if old-number
                       [(make-snailfish (s-first snailfish) splitted) old-number]
                       [snailfish nil]))))
               (if (>= snailfish 10)
                 [(make-snailfish (int (Math/floor (/ snailfish 2))) (int (Math/ceil (/ snailfish 2)))) snailfish]
                 [snailfish nil])))]
    (first (spli snailfish))))

(is (= [[[[0,7],4],[[7,8],[0,13]]],[1,1]] (split-snailfish [[[[0,7],4],[15,[0,13]]],[1,1]])))

(defn reduce-snailfish
  [snailfish]
  (loop [snailfish snailfish]
    ;; (println "R" snailfish)
    (if (nested? snailfish)
      (recur (s-explode snailfish))
      (if (number-greater-than-ten? snailfish)
        (recur (split-snailfish snailfish))
        snailfish))))


(is (= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
       (reduce-snailfish [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                          [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]])))

(is (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]] (reduce-snailfish [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])))

(defn add
  [a b]
  (reduce-snailfish (make-snailfish a b)))

(is (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]] (add [[[[4,3],4],4],[7,[[8,4],9]]] [1,1])))

(is (= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
       (add [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
            [7,[[[3,7],[4,3]],[[6,3],[8,8]]]])))


(defn magnitude
  [v]
  (if (s-vector? v)
    (let [mv (mapv magnitude v)]
      (+ (* 3 (s-first mv)) (* 2 (s-second mv))))
    v))

;; (first (str/split-lines input))
;; (first (parse input))




;; (def input "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
;; [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
;; [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
;; [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
;; [7,[5,[[3,8],[1,4]]]]
;; [[2,[2,2]],[8,[8,1]]]
;; [2,9]
;; [1,[[[9,3],9],[[9,0],[0,7]]]]
;; [[[5,[7,4]],7],1]
;; [[[[4,2],2],6],[8,7]]")

(def answer-1 (magnitude (loop [snailfishes (rest (parse input))
                                res (first (parse input))]
                           (if (empty? snailfishes)
                             res
                             (recur (rest snailfishes) (add res (first snailfishes)))))))

(def parsed (parse input))

(apply max (for [a parsed
                 b parsed
                 :when (not= a b)
                 s [(add a b) (add b a)]]
             (magnitude s)))


    ;;  [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
    ;;  [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
    ;;  [7,[5,[[3,8],[1,4]]]]
    ;;  [[2,[2,2]],[8,[8,1]]]
    ;;  [2,9]
    ;;  [1,[[[9,3],9],[[9,0],[0,7]]]]
    ;;  [[[5,[7,4]],7],1]
    ;;  [[[[4,2],2],6],[8,7]] 
;;  )
;; (defn add-to-left [num value]
;;   (if (number? num)
;;     (+ num value)
;;     [(add-to-left (first num) value) (second num)]))

;; (defn add-to-right [num value]
;;   (if (number? num)
;;     (+ num value)
;;     [(first num) (add-to-right (second num) value)]))

;; (defn explode [num depth]
;;   (cond
;;     ;; Explode condition
;;     (and (vector? num) (= depth 4))
;;     {:exploded true :left (first num) :right (second num) :result 0}

;;     ;; Traverse left and right
;;     (vector? num)
;;     (let [{:keys [exploded left right result]} (explode (first num) (inc depth))]
;;       (if exploded
;;         {:exploded true
;;          :left left
;;          :right nil
;;          :result [(result) (add-to-left (second num) right)]}
;;         (let [{:keys [exploded left right result]} (explode (second num) (inc depth))]
;;           (if exploded
;;             {:exploded true
;;              :left nil
;;              :right right
;;              :result [(add-to-right (first num) left) result]}
;;             {:exploded false :result num}))))

;;     :else {:exploded false :result num}))

;; (defn s-explode [num] (explode num 0))

;; (is (= [[[[0,9],2],3],4] (s-explode [[[[[9,8],1],2],3],4])))
;; (is (= [7,[6,[5,[7,0]]]] (s-explode [7,[6,[5,[4,[3,2]]]]])))
;; (is (= [[6,[5,[7,0]]],3] (s-explode [[6,[5,[4,[3,2]]]],1])))
;; (is (= [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (s-explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])))

;; (is (= [[3 [3 [0 9]]] [6 [5 [4 [3 2]]]]]
;;        (s-explode [[3,[2,[[1 2],7]]],[6,[5,[4,[3,2]]]]])))

;; (is (= [[3,[2,[8,0]]],[9,[5,[7,0]]]] (s-explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])))

;; (defn split [num]
;;   (cond
;;     ;; Split condition
;;     (and (number? num) (>= num 10))
;;     {:split true :result [(int (Math/floor (/ num 2))) (int (Math/ceil (/ num 2)))]}

;;     ;; Traverse left and right
;;     (vector? num)
;;     (let [{:keys [split result]} (split (first num))]
;;       (if split
;;         {:split true :result [result (second num)]}
;;         (let [{:keys [split result]} (split (second num))]
;;           (if split
;;             {:split true :result [(first num) result]}
;;             {:split false :result num}))))

;;     :else {:split false :result num}))

;; (defn reduce-snailfish [num]
;;   (loop [num num]
;;     (let [{:keys [exploded result]} (explode num 0)]
;;       (if exploded
;;         (recur result)
;;         (let [{:keys [split result]} (split num)]
;;           (if split
;;             (recur result)
;;             result))))))

;; (pp/pprint [[[[5 0] [[9 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]] [[[5 [2 8]] 4] [5 [[9 9] 0]]]])


;; ;; Draw the binary tree given as a nested list
;; (defn draw-binary-tree
;;   "Draws a binary tree given as a nested list, visualized in a human-readable format."
;;   [tree]
;;   (letfn [(draw-node [node depth]
;;             (if (coll? node)
;;               (let [left (first node)
;;                     right (second node)
;;                     prefix (str/join (repeat depth "  "))]
;;                 (str prefix "*\n"
;;                      (draw-node left (inc depth))
;;                      (draw-node right (inc depth))))
;;               (str (str/join (repeat depth "  ")) node "\n")))]
;;     (draw-node tree 0)))

;; ;; Example binary tree
;; (def example-tree [[[[5 0] [[9 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]] [[[5 [2 8]] 4] [5 [[9 9] 0]]]])

;; ;; Test the drawing function
;; (println (draw-binary-tree example-tree))
