(ns advent.2020.d10.core
  (:require
   [advent.util :refer [add-items-to-counter add-items-to-counter-times
                        counter]]
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(def example? false)

(def example "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def input (if example? example (slurp "src/advent/2020/d10/input.txt")))

(defn adapters [input]
  (->> input (str/split-lines) (mapv parse-long) sort vec))

(defn device [ratings] (+ 3 (apply max ratings)))

(println
 (let [a (adapters input)
       ratings (conj a (device a))
       diff
       (loop [joltage 0
              ratings ratings
              diff []]
         (if (empty? ratings)
           diff
           (let [f (first ratings)
                 diff (conj diff (- f joltage))
                 joltage f]
             (recur joltage (rest ratings) diff))))]
   (* (->> diff (filter #(= % 3)) count)
      (->> diff (filter #(= % 1)) count))))

(defn- fitting [ratings joltage]
  (if (= joltage (last ratings))
    nil
    (->> ratings (filterv #(and (<= (- % joltage) 3)
                                (> % joltage))))))

(defn combs [ratings]
  (let [f (fitting ratings 0)]
    (loop [D {}
           ways (count f)
           front-cnt (-> (counter) (add-items-to-counter f))
                  ;; cnt 0
           ]
      ;; (println "Ways" ways "FRONT" front-cnt)
      (if (empty? front-cnt)
        ways
        (let [[ff c] (first front-cnt)
              fit (fitting ratings ff)
              D (if (or (contains? D ff) (nil? fit)) D (assoc D ff fit))
              ways (if (some? fit) (+ ways (* c (dec (count (get D ff))))) ways)
              front-cnt (if (some? fit)
                          (add-items-to-counter-times (dissoc front-cnt ff) fit c)
                          (dissoc front-cnt ff))]
          (recur D ways front-cnt))))))

(println (->> input adapters combs))

(is (= 8 (combs (adapters "16
10
15
5
1
11
7
19
6
12
4"))))

(is (= 19208 (combs (adapters example))))

(is (= 4 (combs [1 2 3 6])))
(is (= 10 (combs [1 2 3 5 6])))
(is (= 7 (combs [1 2 3 4])))








