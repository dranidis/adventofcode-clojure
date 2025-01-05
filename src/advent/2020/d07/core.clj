(ns advent.2020.d07.core
  (:require [clojure.string :as str]))

(defn- parse-contents [contents]
  (if (str/starts-with? contents "no other")
    []
    (->> (str/split contents #", ")
         (map (fn [bl]
                (let [[[_ n cls]] (re-seq #"(\d+) (.*) bag" bl)]
                  [(parse-long n) cls]))))))

(def bag-clr-rules
  (->> (slurp "src/advent/2020/d07/input.txt")
       str/split-lines
       (map (fn [r]
              (let [[[_ clr contents]] (re-seq #"(.*) bags contain (.*)" r)
                    contents (parse-contents contents)]
                [clr contents])))
       (into {})))

(defn bag-contains? [bag search-clr]
  (let [bag-cnts (get bag-clr-rules bag)]
    (or (some (fn [[n c]] (= c search-clr)) bag-cnts)
        (some (fn [c] (bag-contains? c search-clr))
              (map (fn [[n c]] c) bag-cnts)))))

(->> (for [[c _] bag-clr-rules
           :when (bag-contains? c "shiny gold")]
       c)
     count
     println)

(defn bat-contains-cnt
  [bag]
  (let [bs (bag-clr-rules bag)]
    (if (empty? bs)
      0
      (+ (->> bs (map first) (apply +))
         (->> bs (map #(* (first %) (bat-contains-cnt (second %))))
              (apply +))))))

(println (bat-contains-cnt "shiny gold"))

