(ns advent.2020.d25.core
  (:require
   [clojure.string :as str]))

(def public-keys (->> (slurp "src/advent/2020/d25/input.txt")
                      str/split-lines
                      (map parse-long)))

(defn loop-size [public-key]
  (->> (loop [v 1
              ls 0]
         (if (= v public-key)
           ls
           (let [v (* v 7)
                 v (rem v 20201227)]
             (recur v (inc ls)))))))

(def lss (map loop-size public-keys))

(->> (loop [v 1
            ls 0]
       (if (= ls (first lss))
         v
         (let [v (* v (second public-keys))
               v (rem v 20201227)]
           (recur v (inc ls)))))
     (println "ANS: "))