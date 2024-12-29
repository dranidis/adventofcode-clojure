(ns advent.2020.d19.core
  (:require
   [advent.util :refer [str->nums]]
   [clojure.string :as str]))

(def example? true)

(def example "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(def input (if example? example (slurp "src/advent/2020/d19/input.txt")))

(def sections (str/split input #"\n\n"))

(defn- parse-rule [line]
  (cond
    (str/includes? line "|")
    (let [[[_ i n1 n2]] (re-seq #"(\d+): (.*) \| (.*)" line)]
      [(parse-long i) {:type :or :data [(->> n1 str->nums) (->> n2 str->nums)]}])

    (str/includes? line "\"")
    (let [[[_ i c]] (re-seq #"(\d+): \"(.)\"" line)]
      [(parse-long i) {:type :c :data c}])

    :else
    (->> line
         str->nums
         ((fn [nums] [(first nums) {:type :seq :data (rest nums)}])))))

(def rules (->> (first sections)
                str/split-lines
                (map parse-rule)
                (into {})))
(def messages (->> (second sections)
                   (str/split-lines)))

(declare matches)

(defn match-seq [rls data s]
  ;; (prn "MTCH" data s)
  (loop [data data
         s s]
    (if (empty? data)
      [true s]
      (let [f (first data)
            [m? rst] (matches rls f s)]
        (if m?
          (recur (rest data) rst)
          [false s])))))

(defn matches [rls rule-nr s]
  (let [rule (get rls rule-nr)]
      ;; (println  "matches:" rule-nr  rule s)
    (let [m (case (:type rule)
              :seq (match-seq rls (:data rule) s)

              :c (if (str/starts-with? s (:data rule))
                   [true (subs s 1)]
                   [false s])

              :or (let [[m? rst] (match-seq rls (get-in rule [:data 0]) s)]
                      ;; (when (#{8 11} rule-nr) 
                      ;;   (println "1st part " rule-nr m? rst))
                    (if m?
                      [m? rst]
                      (let [[m? rst] (match-seq rls (get-in rule [:data 1]) s)]
                          ;; (when (#{8 11} rule-nr)
                          ;;   (println "2nd part " rule-nr m? rst))
                        (if m?
                          [m? rst]
                          [false s])))))]
        ;; (println "R" "matches:" rule-nr  rule s m)
      m)))

(defn matches-all [rules rule-nr s]
  (let [[m? rst] (matches rules rule-nr s)]
    (and m? (empty? rst))))

(->> messages
     (filter (partial matches-all rules 0))
     count
     println)



;; Part 2
(def part-2-rules
  (-> rules
      (assoc 8 {:type :or :data [[42] [42 8]]})
      (assoc 11 {:type :or :data [[42 31] [42 11 31]]})))

;; (part-2-rules 65)
;;               )

;; (part-2-rules 1)

;; (matches-all part-2-rules 0 "babbbbaabbbbbabbbbbbaabaaabaaa")

;; (partition 8 "babbbbaabbbbbabbbbbbaabaaabaaa")
(->> messages
     (filter (partial matches-all part-2-rules 0))
     count
     println)

;; (->> messages
;;      (map count)
;;      distinct)
;; 422 too high
;; 327 too low
;; 316
;; That's not the right answer; your answer is too low. 
;; Curiously, it's the right answer for someone else; 
;; you might be logged in to the wrong account or just unlucky. 
;; In any case, you need to be using your puzzle input. 
;; If you're stuck, make sure you're using the full input data; 
;; there are also some general tips on the about page, 
;; or you can ask for hints on the subreddit. 
;; Please wait one minute before trying again. [Return to Day 19]

(println (->> messages
              (map #(partition 8 %))))