(ns advent.2023.d19.core
  (:require [clojure.string :as str]))

(def input "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

(defn parse [input]
  (let [[lines ratings] (str/split input #"\n\n")]
    [(reduce (fn [a r] (assoc a (get r :rule) (get r :conds)))
             {}
             (for [line (str/split-lines lines)]
               (let [[[_ name rules]] (re-seq #"(.*)\{(.*)\}" line)
                     rules (vec (str/split rules #","))
                     conds (vec (for [rule (pop rules)]
                                  (let [[[_ x op n th]] (re-seq #"(.*)([><=])(\d+):(.*)" rule)]
                                    (str "(" op " " x " " n ") :" th " "))))
                     els (peek rules)]
                 {:rule (keyword name) :conds (str
                                               "(cond "
                                               (apply str conds)
                                               ":else :" els ")")})))
     (for [rating (str/split-lines ratings)]
       (let [values (mapv parse-long (re-seq #"\d+" rating))]
         values))]))

(defn apply-rules [rules part]
  (loop [rule :in]
    ;; (prn rule)
    (if (or (= rule :A) (= rule :R))
      rule
      (let [rule (let [[x m a s] part]
                   (eval (read-string
                          (str "(let ["
                               " x " x
                               " m " m
                               " a " a
                               " s " s
                               " ] "
                               " " (get rules rule)
                               " ) "))))]
        (recur rule)))))

(defn answer-1 [input]
  (let [[rules parts] (parse input)]
    (apply + (flatten (filter (fn [p] (= :A (apply-rules rules p))) parts)))))

;; part 2


(defn index-rules [rules]
  (reduce (fn [acc r] (assoc acc (:rule r) r))
          {}
          rules))

(defn parse-2 [input]
  (let [[lines ratings] (str/split input #"\n\n")]
    [(index-rules
      (for [line (str/split-lines lines)]
        (let [[[_ name rules]] (re-seq #"(.*)\{(.*)\}" line)
              rules (vec (str/split rules #","))
              conds (vec (for [rule (pop rules)]
                           (let [[[_ x op n th]] (re-seq #"(.*)([><=])(\d+):(.*)" rule)]
                                    ;; (str "(" op " " x " " n ") :" th " ")
                             [op x (parse-long n) (keyword th)])))
              els (keyword (peek rules))]
          {:rule (keyword name) :conds
                ;;   (str
                ;;                                "(cond "
                ;;                                (apply str conds)
                ;;                                ":else :" els ")")
           conds :else els})))
     (for [rating (str/split-lines ratings)]
       (let [values (mapv parse-long (re-seq #"\d+" rating))]
         values))]))

;; ;;;;;;;;;;;;;;;;;;;;;;;
;; INTERVALS
;; 

(def max-num 4000)
(def intervals [{:rule :in :intervals {"x" [1 max-num]
                                       "m" [1 max-num]
                                       "a" [1 max-num]
                                       "s" [1 max-num]}}])

(defn get-interval [[op _ n]]
  (if (= op "<")
    [1 (dec n)]
    [(inc n) max-num]))

(defn get-inverse-interval [[op _ n]]
  (if (= op "<")
    [n max-num]
    [1 n]))

(defn interval-intersection [[l1 h1] [l2 h2]]
  (if (< l2 l1)
    (interval-intersection [l2 h2] [l1 h1])
    (cond (> l2 h1) nil
          (> h2 h1) [l2 h1]
          :else [l2 h2])))

(defn split-intervals [interval cnd]
  (let [[_ var _ rule-ind] cnd
        [l1 h1] (get-in interval [:intervals var])
        [l2 h2] (get-interval cnd)
        [l h] (interval-intersection [l1 h1] [l2 h2])
        inverse-interval (interval-intersection (get-inverse-interval cnd)
                                                (get-in interval
                                                        [:intervals
                                                         var]))]
    [(-> interval
         (assoc-in [:intervals var] [l h])
         (assoc-in [:rule] rule-ind))
     [var inverse-interval]]))

(defn- assoc-intervals [interval intervals]
  (reduce (fn [interval i]
            (assoc-in interval [:intervals (first i)] (second i)))
          interval
          intervals))

(defn process-interval [ind-rules interval]
  (let [rule (get ind-rules (:rule interval))
        conds (:conds rule)
        els (:else rule)
        [pr-conds inverse-intervals]
        (reduce (fn [[acc inverse-intrvals] cnd]
                  (let [interval (assoc-intervals interval inverse-intrvals)
                        [intrvl1 other-intrvl] (split-intervals interval cnd)]
                    [(conj acc intrvl1) (conj inverse-intrvals other-intrvl)]))
                [[] []]
                conds)]
    (conj pr-conds
          {:rule els :intervals
           (:intervals (assoc-intervals interval inverse-intervals))})))

(defn process-intervals [ind-rules intervals]
  (flatten (mapv
            (fn [interval]
              (if (#{:A :R} (:rule interval))
                interval
                (process-interval ind-rules interval)))
            intervals)))

(defn all-final? [intervals]
  (every? (fn [i] (#{:A :R} i)) (map :rule intervals)))

(defn process-till-final [ind-rules]
  (loop [intervals intervals]
    (if (all-final? intervals)
      intervals
      (recur (process-intervals ind-rules intervals)))))

(defn answer-2 [input]
  (let [ind-rules (first (parse-2 input))
        final-A (map :intervals
                     (filter #(= :A (get % :rule))
                             (process-till-final ind-rules)))]
    (apply + (map (fn [i]
                    (apply * (map (fn [[l h]]
                                    (inc (- h l)))
                                  (vals i))))
                  final-A))))

(defn -main [& _]
  (let [input (slurp "src/advent/2023/d19/input.txt")]
    (time (println "Day 19 part 1" (answer-1 input)))
    (time (println "Day 19 part 2" (answer-2 input)))))
