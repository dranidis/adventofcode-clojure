(ns advent.2019.d14.core
  (:require
   [advent.util :refer [bin-search]]
   [clojure.string :as str]))

(def ing-out-map
  (->> (str/split-lines (slurp "src/advent/2019/d14/input.txt"))
       (mapv (fn [l]
               (let [[i o] (str/split l #" => ")
                     i (str/split i #", ")
                     i (->> i (mapv #(str/split % #" "))
                            (mapv #(vector (parse-long (first %)) (second %))))
                     o (->> (str/split o #" ")
                            (#(vector (parse-long (first %)) (second %))))]
                 [i o])))
       (map (fn [[i [q o]]]
              [o [q i]]))
       (into {})))

(defn todo [[qty kind]]
  (let [[qty-prod ing] (ing-out-map kind)
        [times remainder] (let [t (quot qty qty-prod)
                                t (if (zero? (mod qty qty-prod)) t (inc t))
                                r (- (* t qty-prod) qty)]
                            [t r])
        ingredient-list (mapv (fn [[q i]] [(* times q) i]) ing)]
    [ingredient-list remainder]))

(defn ore-for-fuel [num]
  (loop [todo-stack [[num "FUEL"]]
         stock {}
         required-ore 0]
    (if (empty? todo-stack)
      required-ore
      (let [[qty kind] (first todo-stack)]
        (if (= kind "ORE")
          (recur (vec (rest todo-stack))
                 stock
                 (+ required-ore qty))
          (let [in-stock (get stock kind)
                [req-qty stock] (if (some? in-stock)
                                  [(- qty in-stock)
                                   (assoc stock kind (if (> in-stock qty) (- in-stock qty) 0))]
                                  [qty stock])
                [ing-list rm] (if (pos? req-qty)
                                (todo [req-qty kind])
                                [[] 0])
                stock (update stock kind (fnil #(+ % rm) 0))]
            (recur (apply conj (vec (rest todo-stack)) ing-list)
                   stock
                   required-ore)))))))


(println "ANS 1: " (ore-for-fuel 1))

(def low-est (long (/ 1000000000000 (ore-for-fuel 1))))

(println "ANS 2:"
         (bin-search low-est (* 2 low-est) #(<= (ore-for-fuel %) 1000000000000)))
