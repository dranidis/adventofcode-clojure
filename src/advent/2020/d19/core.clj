(ns advent.2020.d19.core
  (:require
   [advent.util :refer [str->nums]]
   [clojure.string :as str]))


(def sections (str/split (slurp "src/advent/2020/d19/input.txt") #"\n\n"))

(defn- parse-rule [line]
  (cond
    (str/includes? line "|")
    (let [[[_ i n1 n2]] (re-seq #"(\d+): (.*) \| (.*)" line)]
      [(parse-long i) {:type :or :options [(->> n1 str->nums) (->> n2 str->nums)]}])

    (str/includes? line "\"")
    (let [[[_ i c]] (re-seq #"(\d+): \"(.)\"" line)]
      [(parse-long i) {:type :c :char c}])

    :else
    (->> line
         str->nums
         ((fn [nums] [(first nums) {:type :s :seq (vec (rest nums))}])))))

(def rules (->> (first sections)
                str/split-lines
                (map parse-rule)
                (into {})))
(def messages (->> (second sections)
                   (str/split-lines)))

(defn match-dfs [rules starting-rule-num s]
  (loop [stack [[s [starting-rule-num]]]]
    (if (empty? stack)
      false
      (let [[s rule-nums] (peek stack)
            rule (get rules (first rule-nums))]
        (case (:type rule)
          :c
          (if (str/starts-with? s (:char rule))
            (let [s (subs s 1)
                  rls (rest rule-nums)]
              (if (and (empty? s) (empty? rls))
                true
                (recur (-> (pop stack) (conj [s rls])))))
            (recur (pop stack)))
          :s
          (recur (-> (pop stack)
                     (conj [s (concat (:seq rule) (rest rule-nums))])))

          (recur (-> (pop stack)
                     (#(apply conj %
                              (for [option (:options rule)]
                                [s (concat option (rest rule-nums))]))))))))))

(println "ANS 1:" (->> messages
                       (filter (partial match-dfs rules 0))
                       count))

;; Part 2
(def part-2-rules
  (-> rules
      (assoc 8 {:type :or :options [[42] [42 8]]})
      (assoc 11 {:type :or :options [[42 31] [42 11 31]]})))

(println  "ANS 2:" (->> messages
                        (filter (partial match-dfs part-2-rules 0))
                        count))
