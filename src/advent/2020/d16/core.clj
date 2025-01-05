(ns advent.2020.d16.core
  (:require
   [advent.util :refer [str->nums transpose]]
   [clojure.string :as str]))

(def sections (str/split (slurp "src/advent/2020/d16/input.txt") #"\n\n"))

(def rules
  (->> (first sections)
       (str/split-lines)
       (mapv (fn [line]
               (let [[[_ name f1 t1 f2 t2]]
                     (re-seq #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" line)]
                 [name [(mapv parse-long [f1 t1]) (mapv parse-long [f2 t2])]])))))

(def fields (->> rules (into {})))

(def ticket (->> (second sections) str->nums))

(def nearby (->> (nth sections 2)
                 (str/split-lines)
                 (map str->nums)
                 (remove empty?)))

(def all-intervals (->> rules
                        (map (fn [[_ [r1 r2]]] [r1 r2]))
                        (reduce (fn [a v] (apply conj a v)) [])))

(defn valid? [intervals n]
  (some (fn [[f t]] (<= f n t)) intervals))

;; Part 1
(def invalid-numbers
  (->> nearby
       (apply concat [])
       (remove (partial valid? all-intervals))))

(->> invalid-numbers (apply +) println)

;; Part 2
(def valid-tickets (->> nearby
                        (remove (fn [t] (some #((set invalid-numbers) %) t)))
                        (apply conj [ticket])))

(def possible-classes-for-fields
  (->> (transpose valid-tickets)
       (map (fn [fv]
              (set (for [[n intervals] fields
                         :when (every? (partial valid? intervals) fv)]
                     n))))
       (map-indexed vector)
       vec))

(def field-name-indices
  (loop [mi possible-classes-for-fields
         with-only-one []
         cnt 0]
    (if (or (empty? mi) (= cnt 1000))
      (->> with-only-one (map (fn [[i s]] [i (first s)])))
      (let [[i-once s-once] (->> mi
                                 (filter (fn [[_ s]] (= 1 (count s))))
                                 first)
            pending (conj with-only-one [i-once s-once])
            mi (->> mi (reduce (fn [acc [i s]]
                                 (if (= i-once i)
                                   acc
                                   (conj acc [i (disj s (first s-once))])))
                               []))]
        (recur mi pending (inc cnt))))))

;; Part 2
(->> field-name-indices
     (filter (fn [[i s]] (str/starts-with? s "departure")))
     (map first)
     (map (fn [i] (get ticket i)))
     (apply *)
     println)


