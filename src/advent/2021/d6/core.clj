(ns advent.2021.d6.core)

;; (comment
(def input "3,4,3,1,2")
 ;
  ;; )

(def input (slurp "src/advent/2021/d6/input.txt"))

(defn parse-line
  [input]
  (mapv parse-long (re-seq #"\d+" input)))

(def fishes (assoc (zipmap (range 9)
                           (mapv (fn [n] (count (filter #(= n %) (parse-line input))))
                                 (range 9)))
                   -1 0))
(defn age
  [fishes]
  (reduce (fn [fishes n]
            (let [fishes-of-age (get fishes n)]
              (if (> fishes-of-age 0)
                (-> fishes
                    (update (dec n) (fn [f] (+ f fishes-of-age)))
                    (update n (fn [f] (- f fishes-of-age))))
                fishes)))
          fishes
          (range 9)))

(defn rep
  [fishes]
  (let [parents (get fishes -1)]
    (-> fishes
        (assoc -1 0)
        (update 6 (fn [f] (+ f parents)))
        (update 8 (fn [f] (+ f parents))))))

(defn generation [fishes]
  (-> fishes
      (age)
      (rep)))

(defn answer-1
  [final-day]
  (loop [fishes fishes
         day 0]
    (if (= day final-day)
      (apply + (vals fishes))
      (recur (generation fishes) (inc day)))))

(defn -main [& _]
  (println "Day 1, Part 1:" (answer-1 80))
  (println "Day 1, Part 2:" (answer-1 256)))


