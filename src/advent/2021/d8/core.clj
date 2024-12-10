(ns advent.2021.d8.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.pprint :as pprint]))

(comment
  (def input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
 ;
  )

(comment
  ;; 0  6 segments
  ;; 1  2 segments
  ;; | digit | segments | uniqueness | 
  ;; |-------|----------|------------|
  ;; |     0 |        6 |        | 
  ;; |     1 |        2 | unique |
  ;; |     2 |        5 |        |
  ;; |     3 |        5 |        |
  ;; |     4 |        4 | unique |
  ;; |     5 |        5 |        |
  ;; |     6 |        6 |        |
  ;; |     7 |        3 | unique |
  ;; |     8 |        7 | unique |
  ;; |     9 |        6 |        |

  ;; 6 -> 0 6 9
  ;; 5 -> 2 3 5

  ;
  )

(def input (slurp "src/advent/2021/d8/input.txt"))

(defn parse-line
  [line]
  (let [[segments digits] (str/split line #" *\| *")]
    [(vec (re-seq #"\w+" segments)) (vec (re-seq #"\w+" digits))]))

(defn parse-lines
  [input]
  (vec (for [line (str/split-lines input)]
         (parse-line line))))

(def parsed (parse-lines input))

(def answer-1 (apply + (map
                        (fn [four-digits]
                          (count (filter #(#{2 4 3 7} (count %)) four-digits)))
                        (map second parsed))))

(defn get-mapping
  [line]
  (let
   [xxx (fn
          [unique]
          (set (str/split (first (filter #(= unique (count %)) (flatten line))) #"")))

    ;; digit 1 has only 2 segments and 7 has the same as 1 and an extra segment
;; find this extra segment

    one (xxx 2)
    seven (xxx 3)
    four (xxx 4)
    eight (xxx 7)

    a-segment (first (set/difference seven one))
    b-or-d (set/difference four one)
    e-or-g (set/difference eight (set/union seven four))
    possible-235 (filter #(= 5 (count %)) (first line))
    b-or-e (set (let [yyyy (flatten (map #(str/split % #"") possible-235))]
                  (for [l yyyy
                        :when (= 1 (count (filter #(= l %) yyyy)))]
                    l)))

    c-or-f one
    b-segment (first (set/intersection b-or-e b-or-d))
    d-segment (first (remove #{b-segment} b-or-d))
    e-segment (first (set/intersection e-or-g b-or-e))
    g-segment (first (remove #{e-segment} e-or-g))

    possible-069 (filter #(= 6 (count %)) (first line))
    c-or-d-or-e (set (let [yyyy (flatten (map #(str/split % #"") possible-069))]
                       (for [l yyyy
                             :when (= 2 (count (filter #(= l %) yyyy)))]
                         l)))
    c-segment (first (remove #{d-segment e-segment} c-or-d-or-e))
    f-segment (first (remove #{c-segment} c-or-f))]
    (zipmap [a-segment
             b-segment
             c-segment
             d-segment
             e-segment
             f-segment
             g-segment] ["a" "b" "c" "d" "e" "f" "g"])))


(defn segments->digit
  [segments]
  (case segments
    #{"a" "b" "c" "e" "f" "g"} 0
    #{"c" "f"} 1
    #{"a" "c" "d" "e" "g"} 2
    #{"a" "c" "d" "f" "g"} 3
    #{"b" "c" "d" "f"} 4
    #{"a" "b" "d" "f" "g"} 5
    #{"a" "b" "d" "e" "f" "g"} 6
    #{"a" "c" "f"} 7
    #{"a" "b" "c" "d" "e" "f" "g"} 8
    #{"a" "b" "c" "d" "f" "g"} 9
    :default nil))


(defn four-digit
  [line]
  (let [mapping (get-mapping line)
        digits (map #(str/split % #"") (second line))]
    (parse-long (apply str (map (fn [d] (segments->digit
                                         (set (map mapping d)))) digits)))))

(def answer-2 (apply + (map four-digit parsed)))

(defn -main [& _]
  (println "Day 1, Part 1:" answer-1)
  (println "Day 1, Part 2:" answer-2))

(-main)

