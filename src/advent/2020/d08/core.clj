(ns advent.2020.d08.core
  (:require [clojure.string :as str]))

(def instructions
  (->> (slurp "src/advent/2020/d08/input.txt")
       (str/split-lines)
       (map (fn [l] (let [[[_ cmd n]] (re-seq #"(\w+) ([\+-]\d+)" l)]
                      [cmd (parse-long n)])))
       (map-indexed (fn [idx itm] [idx itm]))
       (into {})))

(defn- exec [acc cnt cmd n]
  (case cmd
    "acc" [(+ acc n) (inc cnt)]
    "jmp" [acc (+ cnt n)]
    "nop" [acc (inc cnt)]
    :noccomand))

(defn- execute [instructions]
  (loop [instructions instructions
         prog-cnt 0
         acc 0
         visited #{}]
    (if (= prog-cnt (count instructions))
      acc
      (if (visited prog-cnt)
        [acc :loop]
        (let [visited (conj visited prog-cnt)
              [cmd n] (get instructions prog-cnt)
              [n-acc n-prog-cnt] (exec  acc prog-cnt cmd n)]
          (recur instructions n-prog-cnt n-acc visited))))))

(->> (execute instructions) first println)

(defn- flip [[cmd n]]
  (if (= "jmp" cmd)
    ["nop" n]
    ["jmp" n]))

(println
 (loop [change-cnt 0]
   (if (= change-cnt (count instructions))
     nil
     (if (= "acc" (first (get instructions change-cnt)))
       (recur (inc change-cnt))
       (let [new-instructions (update instructions change-cnt flip)
             result (execute new-instructions)]
         (if (vector? result)
           (recur (inc change-cnt))
           result))))))