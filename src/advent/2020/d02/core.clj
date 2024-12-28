(ns advent.2020.d02.core
  (:require
   [clojure.string :as str]))

(def rules
  (->> (slurp "src/advent/2020/d02/input.txt")
       str/split-lines
       (map (fn [l]
              (let [[[_ f t c p]] (re-seq #"(\d+)-(\d+) (\w): (\w+)" l)]
                [(parse-long f)
                 (parse-long t)
                 c
                 p])))))

(defn valid-1?
  [[f t c p]]
  (let [cnt (->> p
                 (filter (fn [ch] (= (str ch) c)))
                 count)]
    (<= f cnt t)))


(->> rules
     (filter valid-1?)
     count
     println)

(defn valid-2?
  [[f t c p]]
  (let [at-pos (fn [n] (if (= c (str (nth (vec p) (dec n)))) 1 0))]
    (= 1 (+ (at-pos f) (at-pos t)))))

(->> rules
     (filter valid-2?)
     count
     println)


       


