(ns advent.2020.d14.core
  (:require
   [advent.util :refer [long->bin-str parse-binary]]
   [clojure.string :as str]))

(defn- pad-zeros [bits]
  (vec (concat (repeat (- 36 (count bits)) \0) bits)))

(def cmds
  (->> (str/split-lines "src/advent/2020/d14/input.txt")
       (map (fn [line]
              (let [[l r] (str/split line #" = ")
                    [l r] (if (= l "mask")
                            [:mask (vec r)]
                            [:mem [(-> (re-seq #"\d+" l) first parse-long)
                                   (-> r parse-long long->bin-str vec pad-zeros)]])]
                [l r])))))

(defn- masked [mask val]
  (reduce (fn [v [i b]]
            (if (not= b \X)
              (assoc v i b)
              v))
          val
          (map-indexed vector mask)))

;; Part 1
(defn- update-mem [m mask address v]
  (assoc m address (masked mask v)))

(defn sum-mem-values
  [update-mem]
  (->> (loop [cmds cmds
              m {}
              mask nil]
         (if (empty? cmds)
           m
           (let [[c v] (nth cmds 0)]
             (if (= c :mem)
               (let [[address vl] v]
                 (recur (rest cmds) (update-mem m mask address vl) mask))
               (recur (rest cmds) m v)))))
       vals
       (map (fn [v] (->> v (map str) (apply str) (parse-binary))))
       (apply +)))

(println (sum-mem-values update-mem))

(defn- make-all-addresses [mask]
  (if (empty? mask)
    [[]]
    (let [b (nth mask 0)
          vals (if (= b \X) [\0 \1] [b])]
      (for [v vals
            nxt (make-all-addresses (rest mask))]
        (concat [v] nxt)))))

;; Part 2
(defn- update-mem-2 [m mask address v]
  ;; (prn "update-mem-2" m mask address v)
  (let [masked-address (reduce (fn [v [i b]]
                                 (if (not= b \0)
                                   (assoc v i b)
                                   v))
                               (-> address long->bin-str vec pad-zeros)
                               (map-indexed vector mask))
        addresses (make-all-addresses masked-address)]
    (reduce (fn [m a] (assoc m a v))
            m
            addresses)))

(println (sum-mem-values update-mem-2))
